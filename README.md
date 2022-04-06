## Setup
```sh
dotnet nuget add source https://nuget.pkg.github.com/jesper-bengtson/index.json -n FP2022 -u jesper-bengtson -p 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 --store-password-in-clear-text
```

## Push package
```
dotnet nuget push ScrabbleBot/bin/Debug/TheGrafted.<VERSION HERE>.nupkg --source https://nuget.pkg.github.com/jesper-bengtson/index.json --api-key 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
```
