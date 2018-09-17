# FunkyTetris
*Functional programming & Tetris*

Ideas:  
  - [ ] UI touch-ups (centering, colors, etc)
  - [ ] New favicon
  - [ ] Touch-ups: Rotation tweaks, display in mini boards
  - [x] Lambda Tetromino ;)
  - [ ] Scoring / leveling system
  - [ ] Co-op multiplayer + vs mode (local + websockets?)
  
  
# Building and Running Locally

Contributions are welcome. This project uses the Fable Framework version 1. See [Fable docs](http://fable.io/docs/) for more detailed information.

After cloning, run the following commands in order to get right into it:

1. Install JS dependencies: `yarn install`
2. Move to src folder: `cd src`
3. Install F# dependencies: `dotnet restore`
4. Start Fable daemon and Webpack dev server: `dotnet fable yarn-start`
