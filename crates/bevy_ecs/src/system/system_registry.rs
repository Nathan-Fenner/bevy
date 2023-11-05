use crate::entity::Entity;
use crate::system::{BoxedSystem, Command, IntoSystem};
use crate::world::World;
use crate::{self as bevy_ecs};
use bevy_ecs_macros::Component;
use thiserror::Error;

/// A small wrapper for [`BoxedSystem`] that also keeps track whether or not the system has been initialized.
///
/// The `I` generic parameter is the "input" type for the system, if it takes an [`crate::system::In`] parameter.
///
/// The `O` generic parameter is the output type for the system.
#[derive(Component)]
pub(crate) struct RegisteredSystem<I = (), O = ()> {
    pub(crate) initialized: bool,
    pub(crate) system: BoxedSystem<I, O>,
}

/// A system that has been removed from the registry.
/// It contains the system and whether or not it has been initialized.
///
/// This struct is returned by [`World::remove_system`].
pub struct RemovedSystem {
    initialized: bool,
    system: BoxedSystem,
}

impl RemovedSystem {
    /// Is the system initialized?
    /// A system is initialized the first time it's ran.
    pub fn initialized(&self) -> bool {
        self.initialized
    }

    /// The system removed from the storage.
    pub fn system(self) -> BoxedSystem {
        self.system
    }
}

/// An identifier for a registered system.
///
/// These are opaque identifiers, keyed to a specific [`World`],
/// and are created via [`World::register_system`].
#[derive(PartialEq, Eq, Hash)]
pub struct SystemId<I = (), O = ()>(
    pub(crate) Entity,
    pub(crate) std::marker::PhantomData<fn(I) -> O>,
);

// The default bounds on the derived `Clone` for `SystemId` would require that `I: Clone` as well.
// Therefore, a custom impl is provided instead.
impl<I, O> Clone for SystemId<I, O> {
    fn clone(&self) -> Self {
        Self(self.0, self.1)
    }
}

impl<I, O> std::fmt::Debug for SystemId<I, O> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // The PhantomData is deliberately omitted to simplify the debug string.
        f.debug_tuple("SystemId").field(&self.0).finish()
    }
}

// The default bounds on the derived `Copy` for `SystemId` would require that `I: Copy` as well.
// Therefore, a custom impl is provided instead.
impl<I, O> Copy for SystemId<I, O> {}

impl World {
    /// Registers a system and returns a [`SystemId`] so it can later be called by [`World::run_system`].
    ///
    /// It's possible to register the same systems more than once, they'll be stored separately.
    ///
    /// This is different from adding systems to a [`Schedule`](crate::schedule::Schedule),
    /// because the [`SystemId`] that is returned can be used anywhere in the [`World`] to run the associated system.
    /// This allows for running systems in a pushed-based fashion.
    /// Using a [`Schedule`](crate::schedule::Schedule) is still preferred for most cases
    /// due to its better performance and abillity to run non-conflicting systems simultaneously.
    ///
    /// If the system takes an input parameter, this is reflected in the return type as `SystemId<I>`.
    pub fn register_system<M, I: 'static, O: 'static, S: IntoSystem<I, O, M> + 'static>(
        &mut self,
        system: S,
    ) -> SystemId<I, O> {
        SystemId(
            self.spawn(RegisteredSystem {
                initialized: false,
                system: Box::new(IntoSystem::into_system(system)),
            })
            .id(),
            std::marker::PhantomData,
        )
    }

    /// Removes a registered system and returns the system, if it exists.
    /// After removing a system, the [`SystemId`] becomes invalid and attempting to use it afterwards will result in errors.
    /// Re-adding the removed system will register it on a new [`SystemId`].
    ///
    /// If no system corresponds to the given [`SystemId`], this method returns an error.
    /// Systems are also not allowed to remove themselves, this returns an error too.
    pub fn remove_system<I, O>(
        &mut self,
        id: SystemId<I, O>,
    ) -> Result<RemovedSystem, RegisteredSystemError<I, O>> {
        match self.get_entity_mut(id.0) {
            Some(mut entity) => {
                let registered_system = entity
                    .take::<RegisteredSystem>()
                    .ok_or(RegisteredSystemError::SelfRemove(id))?;
                entity.despawn();
                Ok(RemovedSystem {
                    initialized: registered_system.initialized,
                    system: registered_system.system,
                })
            }
            None => Err(RegisteredSystemError::SystemIdNotRegistered(id)),
        }
    }

    /// Run stored systems by their [`SystemId`].
    /// Before running a system, it must first be registered.
    /// The method [`World::register_system`] stores a given system and returns a [`SystemId`].
    /// This is different from [`RunSystemOnce::run_system_once`](crate::system::RunSystemOnce::run_system_once),
    /// because it keeps local state between calls and change detection works correctly.
    ///
    /// To call a chained system that takes an input, use [`World::run_system_with_input`] instead.
    ///
    /// # Limitations
    ///  - Stored systems cannot be recursive, they cannot call themselves through [`Commands::run_system`](crate::system::Commands).
    ///  - Exclusive systems cannot be used.
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use bevy_ecs::prelude::*;
    /// #[derive(Resource, Default)]
    /// struct Counter(u8);
    ///
    /// fn increment(mut counter: Local<Counter>) {
    ///    counter.0 += 1;
    ///    println!("{}", counter.0);
    /// }
    ///
    /// let mut world = World::default();
    /// let counter_one = world.register_system(increment);
    /// let counter_two = world.register_system(increment);
    /// world.run_system(counter_one); // -> 1
    /// world.run_system(counter_one); // -> 2
    /// world.run_system(counter_two); // -> 1
    /// ```
    ///
    /// Change detection:
    ///
    /// ```rust
    /// # use bevy_ecs::prelude::*;
    /// #[derive(Resource, Default)]
    /// struct ChangeDetector;
    ///
    /// let mut world = World::default();
    /// world.init_resource::<ChangeDetector>();
    /// let detector = world.register_system(|change_detector: ResMut<ChangeDetector>| {
    ///     if change_detector.is_changed() {
    ///         println!("Something happened!");
    ///     } else {
    ///         println!("Nothing happened.");
    ///     }
    /// });
    ///
    /// // Resources are changed when they are first added
    /// let _ = world.run_system(detector); // -> Something happened!
    /// let _ = world.run_system(detector); // -> Nothing happened.
    /// world.resource_mut::<ChangeDetector>().set_changed();
    /// let _ = world.run_system(detector); // -> Something happened!
    /// ```
    pub fn run_system<O: 'static>(
        &mut self,
        id: SystemId<(), O>,
    ) -> Result<O, RegisteredSystemError<(), O>> {
        self.run_system_with_input(id, ())
    }

    /// Run stored systems by their [`SystemId`].
    /// Before running a system, it must first be registered.
    /// The method [`World::register_system`] stores a given system and returns a [`SystemId`].
    ///
    /// `run_system_with_input` behaves just like [`World::run_system`], but also allows an
    /// input value to be provided to the registered system, if it requires one.
    ///
    /// ```rust
    /// # use bevy_ecs::prelude::*;
    /// #[derive(Resource, Default)]
    /// struct Counter(u8);
    ///
    /// fn increment(In(increment_by): In<u8>, mut counter: Local<Counter>) {
    ///    counter.0 += increment_by;
    ///    println!("{}", counter.0);
    /// }
    ///
    /// let mut world = World::default();
    /// let counter_one = world.register_system(increment);
    /// let counter_two = world.register_system(increment);
    /// world.run_system_with_input(counter_one, 2); // -> 2
    /// world.run_system_with_input(counter_one, 100); // -> 102
    /// world.run_system(counter_two, 1); // -> 103
    /// ```
    /// See [`World::run_system`] for more details about running stored systems.
    pub fn run_system_with_input<Input: 'static, Output: 'static>(
        &mut self,
        id: SystemId<Input, Output>,
        input_value: Input,
    ) -> Result<Output, RegisteredSystemError<Input, Output>> {
        // lookup
        let mut entity = self
            .get_entity_mut(id.0)
            .ok_or(RegisteredSystemError::SystemIdNotRegistered(id))?;

        // take ownership of system trait object
        let RegisteredSystem {
            mut initialized,
            mut system,
        } = entity
            .take::<RegisteredSystem<Input, Output>>()
            .ok_or(RegisteredSystemError::Recursive(id))?;

        // run the system
        if !initialized {
            system.initialize(self);
            initialized = true;
        }
        let output_value = system.run(input_value, self);
        system.apply_deferred(self);

        // return ownership of system trait object (if entity still exists)
        if let Some(mut entity) = self.get_entity_mut(id.0) {
            entity.insert::<RegisteredSystem<Input, Output>>(RegisteredSystem {
                initialized,
                system,
            });
        }
        Ok(output_value)
    }
}

/// The [`Command`] type for [`World::run_system`].
///
/// This command runs systems in an exclusive and single threaded way.
/// Running slow systems can become a bottleneck.
#[derive(Debug, Clone)]
pub struct RunSystem {
    system_id: SystemId,
}

impl RunSystem {
    /// Creates a new [`Command`] struct, which can be added to [`Commands`](crate::system::Commands)
    pub fn new(system_id: SystemId) -> Self {
        Self { system_id }
    }
}

impl Command for RunSystem {
    #[inline]
    fn apply(self, world: &mut World) {
        let _ = world.run_system(self.system_id);
    }
}

/// An operation with stored systems failed.
#[derive(Error)]
pub enum RegisteredSystemError<I = (), O = ()> {
    /// A system was run by id, but no system with that id was found.
    ///
    /// Did you forget to register it?
    #[error("System {0:?} was not registered")]
    SystemIdNotRegistered(SystemId<I, O>),
    /// A system tried to run itself recursively.
    #[error("System {0:?} tried to run itself recursively")]
    Recursive(SystemId<I, O>),
    /// A system tried to remove itself.
    #[error("System {0:?} tried to remove itself")]
    SelfRemove(SystemId<I, O>),
}

impl<I, O> std::fmt::Debug for RegisteredSystemError<I, O> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::SystemIdNotRegistered(id) => {
                f.debug_tuple("SystemIdNotRegistered").field(id).finish()
            }
            Self::Recursive(id) => f.debug_tuple("Recursive").field(id).finish(),
            Self::SelfRemove(id) => f.debug_tuple("SelfRemove").field(id).finish(),
        }
    }
}

mod tests {
    use crate as bevy_ecs;
    use crate::prelude::*;

    #[derive(Resource, Default, PartialEq, Debug)]
    struct Counter(u8);

    #[test]
    fn change_detection() {
        #[derive(Resource, Default)]
        struct ChangeDetector;

        fn count_up_iff_changed(
            mut counter: ResMut<Counter>,
            change_detector: ResMut<ChangeDetector>,
        ) {
            if change_detector.is_changed() {
                counter.0 += 1;
            }
        }

        let mut world = World::new();
        world.init_resource::<ChangeDetector>();
        world.init_resource::<Counter>();
        assert_eq!(*world.resource::<Counter>(), Counter(0));
        // Resources are changed when they are first added.
        let id = world.register_system(count_up_iff_changed);
        let _ = world.run_system(id);
        assert_eq!(*world.resource::<Counter>(), Counter(1));
        // Nothing changed
        let _ = world.run_system(id);
        assert_eq!(*world.resource::<Counter>(), Counter(1));
        // Making a change
        world.resource_mut::<ChangeDetector>().set_changed();
        let _ = world.run_system(id);
        assert_eq!(*world.resource::<Counter>(), Counter(2));
    }

    #[test]
    fn one_shot_system_with_input() {
        #[derive(Resource, Default, Eq, PartialEq, Debug)]
        struct Counter(i32);

        // Demonstration that we can pass in non-Copy, non-Clone types.
        struct NonCopyInt(i32);

        fn count_up_by(increment_by: In<NonCopyInt>, mut counter: ResMut<Counter>) {
            counter.0 += increment_by.0 .0;
        }

        let mut world = World::new();
        world.init_resource::<Counter>();

        let id = world.register_system(count_up_by);

        // Increase by 5
        world
            .run_system_with_input(id, NonCopyInt(5))
            .expect("no error from running system");
        assert_eq!(*world.resource::<Counter>(), Counter(5));

        // Increase by 100
        world
            .run_system_with_input(id, NonCopyInt(100))
            .expect("no error from running system");
        assert_eq!(*world.resource::<Counter>(), Counter(105));
    }

    #[test]
    fn one_shot_system_with_output() {
        #[derive(Resource, Default, Eq, PartialEq, Debug)]
        struct Counter(i32);

        // Demonstration that we can return non-Copy, non-Clone types.
        #[derive(Eq, PartialEq, Debug)]
        struct NonCopyInt(i32);

        fn count_up(mut counter: ResMut<Counter>) -> NonCopyInt {
            counter.0 += 1;
            NonCopyInt(counter.0)
        }

        let mut world = World::new();
        world.init_resource::<Counter>();

        let id = world.register_system(count_up);

        // Increase by 1
        let out = world.run_system(id).expect("no error from running system");
        assert_eq!(*world.resource::<Counter>(), Counter(1));
        assert_eq!(out, NonCopyInt(1));

        // Increase by 1 again
        let _ = world.run_system(id);
        assert_eq!(*world.resource::<Counter>(), Counter(15));
    }

    #[test]
    fn creating_system_from_commands() {
        #[derive(Resource, Default, Eq, PartialEq, Debug)]
        struct Counter(i32);

        fn count_up(mut counter: ResMut<Counter>) {
            counter.0 += 1;
        }

        fn sys_that_registers_sys(mut commands: Commands) -> bevy_ecs::system::SystemId {
            commands.register_system(count_up)
        }

        let mut world = World::new();

        let meta_register_sys_id = world.register_system(sys_that_registers_sys);
        let count_sys_id = world
            .run_system(meta_register_sys_id)
            .expect("no system error");

        // After the system has been registered, create its required resource.
        world.init_resource::<Counter>();
        assert_eq!(*world.resource::<Counter>(), Counter(0));

        // Increase by 1
        world.run_system(count_sys_id).expect("no system error");
        assert_eq!(*world.resource::<Counter>(), Counter(1));

        // Increase by 1 again
        world.run_system(count_sys_id).expect("no system error");
        assert_eq!(*world.resource::<Counter>(), Counter(2));

        // Increase by 1 again
        world.run_system(count_sys_id).expect("no system error");
        assert_eq!(*world.resource::<Counter>(), Counter(3));
    }

    #[test]
    fn one_shot_with_lazily_initialized_state() {
        #[derive(Resource, Default, Eq, PartialEq, Debug)]
        struct Counter(i32);

        // Demonstration that we can return non-Copy, non-Clone types.
        #[derive(Eq, PartialEq, Debug)]
        struct NonCopyInt(i32);

        fn count_up(mut counter: ResMut<Counter>) {
            counter.0 += 1;
        }

        let mut world = World::new();

        let id = world.register_system(count_up);

        world.init_resource::<Counter>();

        // Increase by 1
        world.run_system(id).expect("no error from running system");
        assert_eq!(*world.resource::<Counter>(), Counter(1));

        // Increase by 1 again
        world.run_system(id).expect("no error from running system");
        assert_eq!(*world.resource::<Counter>(), Counter(2));
    }

    #[test]
    fn local_variables() {
        // The `Local` begins at the default value of 0
        fn doubling(last_counter: Local<Counter>, mut counter: ResMut<Counter>) {
            counter.0 += last_counter.0 .0;
            last_counter.0 .0 = counter.0;
        }

        let mut world = World::new();
        world.insert_resource(Counter(1));
        assert_eq!(*world.resource::<Counter>(), Counter(1));
        let id = world.register_system(doubling);
        let _ = world.run_system(id);
        assert_eq!(*world.resource::<Counter>(), Counter(1));
        let _ = world.run_system(id);
        assert_eq!(*world.resource::<Counter>(), Counter(2));
        let _ = world.run_system(id);
        assert_eq!(*world.resource::<Counter>(), Counter(4));
        let _ = world.run_system(id);
        assert_eq!(*world.resource::<Counter>(), Counter(8));
    }

    #[test]
    fn nested_systems() {
        use crate::system::SystemId;

        #[derive(Component)]
        struct Callback(SystemId);

        fn nested(query: Query<&Callback>, mut commands: Commands) {
            for callback in query.iter() {
                commands.run_system(callback.0);
            }
        }

        let mut world = World::new();
        world.insert_resource(Counter(0));

        let increment_two = world.register_system(|mut counter: ResMut<Counter>| {
            counter.0 += 2;
        });
        let increment_three = world.register_system(|mut counter: ResMut<Counter>| {
            counter.0 += 3;
        });
        let nested_id = world.register_system(nested);

        world.spawn(Callback(increment_two));
        world.spawn(Callback(increment_three));
        let _ = world.run_system(nested_id);
        assert_eq!(*world.resource::<Counter>(), Counter(5));
    }
}
