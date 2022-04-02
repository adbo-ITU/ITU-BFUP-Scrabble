## Setup
```sh
dotnet nuget add source https://nuget.pkg.github.com/jesper-bengtson/index.json -n FP2022 -u jesper-bengtson -p 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 --store-password-in-clear-text
```

## Push package
```
dotnet nuget push ScrabbleBot/bin/Debug/TheGrafted.<VERSION HERE>.nupkg --source https://nuget.pkg.github.com/jesper-bengtson/index.json --api-key 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
```
