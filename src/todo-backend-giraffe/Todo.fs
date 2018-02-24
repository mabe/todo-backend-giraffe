module Todo
open System

type Todo =
    { Id: int
      Url : string
      Title : string
      Completed : bool
      Order : int }
    with
    static member Empty = { Id = -1; Url = ""; Title = "EMPTY"; Completed = false; Order = -1 }

type TodoPatch() =
    member val Title : string = null with get, set
    member val Completed : Nullable<bool> = Nullable() with get, set
    member val Order : Nullable<int> = Nullable() with get, set
//type TodoPatch =
//     { Title : string option
//       Completed : bool option
//       Order : int option }

type IContainer =
    abstract GetAll : unit -> Async<Todo[]>
    abstract Post : todo: Todo -> Async<int> 
    abstract Clear : unit -> unit
    abstract Get : index: int -> Async<Todo option>
    abstract Update : index: int * patch: TodoPatch -> Async<Todo option>
    abstract Remove : index: int -> Async<unit option>

module InMemory =
    type Agent<'T> = MailboxProcessor<'T>

    type TodoOperation =
        | GetAll of channel: AsyncReplyChannel<Todo[]>
        | Post of todo: Todo * channel: AsyncReplyChannel<int>
        | Clear
        | Get of index: int * channel: AsyncReplyChannel<Todo option>
        | Update of index: int * patch: TodoPatch * channel: AsyncReplyChannel<Todo option>
        | Remove of index: int * channel: AsyncReplyChannel<unit option>

    let store = 
        let getTodo index (todos: Todo[]) =
            if todos.Length > index then
                let todo = todos.[index]
                if todo = Todo.Empty then
                    None
                else Some todo
            else None

        let agent =
            Agent<_>.Start(fun inbox -> 
                let rec loop todos = 
                    async { 
                        let! msg = inbox.Receive()
                        match msg with
                        | GetAll ch -> 
                            ch.Reply (todos |> Array.filter (fun x -> x <> Todo.Empty))
                            return! loop todos
                        | Post(todo, ch) -> 
                            let index = todos.Length
                            ch.Reply index
                            return! loop (Array.append todos [| { todo with Id = index } |])
                        | Clear -> 
                            return! loop [||]
                        | Get(index, ch) ->
                            let todo = getTodo index todos
                            ch.Reply todo
                            return! loop todos
                        | Update(index, todo, ch) ->
                            let todo =
                                match getTodo index todos with
                                | Some temp ->
                                    let update =
                                        { temp with
                                            Id = index
                                            Title = match todo.Title with | null -> temp.Title | _ -> todo.Title
                                            Completed = match todo.Completed.HasValue with | false -> temp.Completed | _ -> todo.Completed.Value
                                            Order = match todo.Order.HasValue with | false -> temp.Order | _ -> todo.Order.Value }
                                    todos.[index] <- update
                                    Some update
                                | None -> None
                            ch.Reply todo
                            return! loop todos
                        | Remove(index, ch) ->
                            let result = 
                                match getTodo index todos with
                                | Some _ ->
                                    todos.[index] <- Todo.Empty
                                    Some ()
                                | None -> None
                            ch.Reply result
                            return! loop todos
                    }
                loop [||])

        { new IContainer with
            member __.GetAll() = agent.PostAndAsyncReply GetAll
            member __.Post(todo) = agent.PostAndAsyncReply(fun ch -> Post(todo, ch))
            member __.Clear() = agent.Post Clear
            member __.Get(index) = agent.PostAndAsyncReply(fun ch -> Get(index, ch))
            member __.Update(index, patch) = agent.PostAndAsyncReply(fun ch -> Update(index, patch, ch))
            member __.Remove(index) = agent.PostAndAsyncReply(fun ch -> Remove(index, ch)) }
