# monad-fork2

This library offers a typeclass and instances to simplify the process of multithreading within monad transformer stacks.

Transformers which work within this class will have one or more of the following effects:
* Requiring an environment or initial state from the parent thread (ReaderT, StateT, RWST)
* A result other than success (MaybeT, ExceptT)
* Additional data upon completion (WriterT, StateT, RWST)

Effects of the first kind are trivial, the environment is retreived within the monad, and used to fork the new thread.

Effects of the second and third kind are less trivial, they are solved by passing a "handler" to the fork function. Effects of the second kind are handled by functions which take the non-standard exit, and the third kind are handled by functions which take the additional data.

Stacks of tranformers are then handled by sums of their handlers (accomplished with the :<: operator), with the base always being IO, which requires no handler, but requires a value of type () for consitency.

A handler might send a message to the main thread using the structures in Control.Concurrent, or the STM library, write to a file handle, or perhaps ignore the result altogether.

## Usage

To use this library after building, import the Control.Monad.Fork module, and either specify or derive and instance of MonadFork.

Actions within that monad can then be run in a new thread using the forkT function.

Instances are provided for the following mtl transformers, so any stack of them should automatically have an instance defined:

* MaybeT: Arity-0 handler fires on Nothing result
* ExceptT: Arity-1 handler fires on Left result, and takes the exception value as an argument
* ReaderT: No handler, but provides environment to child thread
* WriterT: Arity-1 handler fires on completion, and takes the final monoidal value
* StateT: Arity-2 handler fires on completion, and takes both the initial and final state values, for potential "diff"ing purposes
* RWST: Combines actions of ReaderT, WriterT, and StateT like you would expect
