module TodoBackendGiraffe.App

open System
open System.IO
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Cors.Infrastructure
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Logging
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting
open Giraffe
open FSharp.Control.Tasks.V2

open Todo.InMemory

let setTodoUrl (ctx: HttpContext) (todo: Todo.Todo) = { todo with Url = sprintf "%s://%A/todo/%i" ctx.Request.Scheme ctx.Request.Host todo.Id }

let found ctx = function
    | Some (todo: Todo.Todo) -> json (setTodoUrl ctx todo)
    | None -> RequestErrors.notFound (text "Not found")

let getAll = fun next ctx -> task {
    let! todos = store.GetAll () |> Async.StartAsTask

    return! json (todos |> Seq.map (setTodoUrl ctx)) next ctx
}

let get = fun i next ctx -> task {
    let! todo = (store.Get i |> Async.StartAsTask)

    return! (found ctx todo) next ctx
}

let patch = fun id next (ctx: HttpContext) -> task {
    let! patch = ctx.BindJsonAsync<Todo.TodoPatch>()
    let! todo = store.Update (id, patch) |> Async.StartAsTask

    return! (found ctx (todo)) next ctx
}

let post = fun next (ctx: HttpContext) -> task {
    let! todo = ctx.BindModelAsync<Todo.Todo>()
    let! index = store.Post todo |> Async.StartAsTask
    let todo' = setTodoUrl ctx ({ todo with Id = index; }) 

    return! (setHttpHeader "Location" todo'.Url >=> Successful.created (json todo')) next ctx
}

let deleteAll = fun next ctx -> 
    store.Clear ()
    Successful.ok (text "") next ctx

let delete = fun i next ctx -> task {
    let! index = store.Remove i |> Async.StartAsTask

    return! 
        (match index with
            | Some _ -> (setStatusCode 204)
            | None -> RequestErrors.notFound (text "Not Found")) next ctx
}

let webApp =
    choose [
        GET >=>
            choose [
                route "/" >=> getAll
                route "/todos" >=> getAll
                routef "/todo/%i" get
            ]
        PATCH >=> 
            choose [
                routef "/todo/%i" patch
            ]
        POST >=>
            choose [
                route "/" >=> post
            ]        
        DELETE >=>
            choose [
                route "/" >=> deleteAll
                route "/todos" >=> deleteAll
                routef "/todo/%i" delete
            ]

        setStatusCode 404 >=> text "Not Found" ]

// ---------------------------------
// Error handler
// ---------------------------------

let errorHandler (ex : Exception) (logger : ILogger) =
    logger.LogError(EventId(), ex, "An unhandled exception has occurred while executing the request.")
    clearResponse >=> setStatusCode 500 >=> text ex.Message

// ---------------------------------
// Config and Main
// ---------------------------------

let configureCors (builder : CorsPolicyBuilder) =
    builder.WithOrigins("*")
           .WithMethods([|"GET";"HEAD";"OPTIONS";"PATCH";"POST";"DELETE";"PUT"|])
           .WithHeaders([|"Accept"; "Origin"; "Content-type"|])
           |> ignore

let configureApp (app : IApplicationBuilder) =
    let env = app.ApplicationServices.GetService<IWebHostEnvironment>()
    (match env.IsDevelopment() with
    | true  -> app.UseDeveloperExceptionPage()
    | false -> app.UseGiraffeErrorHandler errorHandler)
        .UseCors(configureCors)
        .UseStaticFiles()
        .UseGiraffe(webApp)

let configureServices (services : IServiceCollection) =
    services.AddCors()    |> ignore
    services.AddGiraffe() |> ignore

let configureLogging (builder : ILoggingBuilder) =
    let filter (l : LogLevel) = l.Equals LogLevel.Error
    builder.AddFilter(filter).AddConsole().AddDebug() |> ignore

[<EntryPoint>]
let main _ =
    let contentRoot = Directory.GetCurrentDirectory()
    WebHostBuilder()
        .UseKestrel()
        .UseContentRoot(contentRoot)
        .UseIISIntegration()
        .Configure(Action<IApplicationBuilder> configureApp)
        .ConfigureServices(configureServices)
        .ConfigureLogging(configureLogging)
        .Build()
        .Run()
    0