# The Scrabble Project
The final project for the Functional Programming course at ITU.

Group members:
- Adrian Borup (adbo@itu.dk)
- Joachim Borup (aljb@itu.dk)

## Points we've aimed for
- [x] 2 points - Finish a game against yourself on an infinite board (you do not have to count points nor play well)
- [x] 1 point - Multiplayer and implement your dictionary
- [x] 1 point - Parellise your algorithm
- [ ] 1 point - Use the DSL to parse the board and calculate points
- [x] 1 point - Respect the timeout flag

## How to run
### Using .NET
Requires .NET v6 to be installed. Our `dotnet --version` outputs `6.0.104`. To run:
```bash
cd ScrabbleTemplate
dotnet run
```

### Using Docker
To be completely sure that you (likely the TA grading this project) can run this project, we've provided a `Dockerfile` that can be used.

To run the project using Docker, simply run the following two commands:
```bash
docker build -t scrabble .
docker run -it --rm scrabble
```
