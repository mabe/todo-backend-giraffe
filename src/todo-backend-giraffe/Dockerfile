FROM mcr.microsoft.com/dotnet/core/sdk:3.1-alpine3.10 AS build-env
WORKDIR /app

# Copy csproj and restore as distinct layers
COPY *.fsproj ./
RUN dotnet restore

# Copy everything else and build
COPY . ./
RUN dotnet publish -c Release -o out

# Build runtime image
FROM mcr.microsoft.com/dotnet/core/aspnet:3.1-alpine3.10
WORKDIR /app
COPY --from=build-env /app/out .
# ENTRYPOINT ["dotnet", "TodoBackendGiraffe.dll"]
CMD ASPNETCORE_URLS=http://*:$PORT dotnet TodoBackendGiraffe.dll
