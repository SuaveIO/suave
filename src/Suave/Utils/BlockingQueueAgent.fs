namespace Suave.Utils

type BlockingAgentMessage<'T> = 
  | Get of AsyncReplyChannel<'T>
  | Add of 'T * AsyncReplyChannel<unit> 

open System.Collections.Generic

/// Agent-based implementation of producer/consumer problem 
type BlockingQueueAgent<'T>() =
    let agent = MailboxProcessor.Start(fun agent ->
      let queue = new Queue<_>()

      // State machine running inside the agent
      let rec emptyQueue() = 
        agent.Scan(fun msg ->
            match msg with 
            | Add(value, reply) -> Some(enqueueAndContinue(value, reply))
            | _ -> None )
      and fullQueue() = 
        agent.Scan(fun msg ->
            match msg with 
            | Get(reply) -> Some(dequeueAndContinue(reply))
            | _ -> None )
      and runningQueue() = async {
        let! msg = agent.Receive() 
        match msg with 
        | Add(value, reply) -> return! enqueueAndContinue(value, reply)
        | Get(reply) -> return! dequeueAndContinue(reply) }
      and enqueueAndContinue (value, reply) = async {
        queue.Enqueue(value)
        reply.Reply() 
        return! chooseState() }
      and dequeueAndContinue (reply) = async { 
        reply.Reply(queue.Dequeue())
        return! chooseState() }
      and chooseState() = 
        if queue.Count = 0 then emptyQueue()
        else runningQueue()
      // Start with an empty queue
      emptyQueue())

    /// Asynchronously adds item to the queue. If the queue
    /// is full, it blocks until some items are removed.
    member x.AsyncAdd(v:'T) = 
        agent.PostAndAsyncReply(fun ch -> Add(v, ch))

    /// Asynchronously gets item from the queue. If the queue
    /// is empty, it blocks until some items are added.
    member x.AsyncGet() = 
        agent.PostAndAsyncReply(Get)
