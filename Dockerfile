# Mostly taken from https://docs.microsoft.com/en-us/dotnet/core/docker/build-container?tabs=windows
FROM mcr.microsoft.com/dotnet/sdk:6.0 AS build-env
WORKDIR /app

# Copy everything
COPY . ./
# Build and publish a release
RUN dotnet publish -c Release -o out

# Build runtime image
FROM mcr.microsoft.com/dotnet/runtime:6.0
WORKDIR /app
COPY --from=build-env /app/out .
COPY --from=build-env /app/ScrabbleTemplate/Dictionaries/English.txt ./Dictionaries/English.txt
ENTRYPOINT ["dotnet", "ScrabbleTemplate.dll"]
