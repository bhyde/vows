# Vows

Vows is a common lisp package which layers on top of bordeaux-threads.  Currently very rough.

Playing with the ideas mentioned here
<http://en.wikipedia.org/wiki/Futures_and_promises>.  But only enough
to get a tool needed for some simple parallelism in yet another
experiment.

## Dictionary

<b>vow:vow</b> is a class which represents a promise to perform some work.  Access to
this class should be done while holding the bt:lock found in it's vow::lock slot.
It's vow::run-state slot maybe queried to get a sense of it's status; see the code.

<b>vow::promise-keeper</b> is a class consisting of a list of promises to keep and a pool
of threads which labor to fulfil those promises.

<b>vow:*promise-keeper*</b> is the current promise keeper, and instance of vow::promise-keeper.

<b>vow:vow lambda</b> returns a vow:vow representing the promise to do the work of
the lambda after adding it to the todo list of the current promise keeper.

<b>vow:fulfiled? vow</b> nil or the vow if it has been fufilled.

<b>vow:fulfil vow</b> blocks until the given vow has been fufilled and then returns it's result,
which may have multiple values or be an error.

<b>vow:find-fulfilled vows</b> returns a vow from the list vows that has been fufilled, blocking
if necessary.

<b>vow:broken-vow</b> the subclass of simple-error used to denote the condition of created with by vow:break-vow.

<b>vow:break-vow vow</b> Fulfils the vow as if it had suffered the simple error vow:broken-vow.  Blocks
until it can gain a lock on the promise keeper.  It then either removes the vow from his to do list,
or if one of his minions is currently working on keeping this promise it forces that attempt to
unwind.  It marks the vow as having been fufill by the error vow:broken-vow, even if the vow
is already fufilled.

## Missing stuff and notes

### Missing: Triggers
Because the above design encourages blocking it is bogus.

Instead, as in E, fulfillment should just trigger the running of
continuation code.  Possibly this is something like:

<b>trigger</b> a datum representing some work to enqueue once some promise
has been fulfilled.  The trigger may run immediately, later when the
promise is fulfilled.  Note the trigger can run when the promise is
broken.

<b>vow:add-trigger vow lambda</b> a method on a vow that creates and returns
an additional trigger on that vow.

<b>vow:remove-trigger vow trigger</b> a method on a vow.  returns nil.  after
calling this the trigger will no longer be pending for that vow.

### Missing abstractions on sets of vows.

There are some patterns vow/tigger/continuations that should be given
first class status.  In particular various triggering patterns on the
fulfillment of sets of vows.

* N triggers
    * for each vow fulfilled trigger
* 1 trigger
    * when all vows fulfilled trigger
    * when any one fulfilled trigger, and then break the other vows.

### Promise keeper

I doubt I've got the right design for the promise keeper.  He's
entirely naive, as written, about what order to work on his todo
list and that seems extremely unlikely to be the right approach.

In fact I'm confident that it would be possible to have all threads
in the pool blocked on I/O while there are other items in the 

### Affordances

In theory it should be possible to implement subclasses of promise-keeper
and promise.
