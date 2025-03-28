# Pong in Haskell using SDL

![original pong game by Atari](https://upload.wikimedia.org/wikipedia/commons/6/62/Pong_Game_Test2.gif)

This project is a Haskell implementation of the classic Pong game using SDL2 and its Haskell bindings. Inspired by the [original 1972 Pong](https://en.wikipedia.org/wiki/Pong) by Atari, this single-player version challenges you to control a paddle and bounce the ball against the wall as many times as possible.

## Overview

Pong is one of the first arcade games ever created. In this implementation:
- **Game Loop:** A dynamic framerate game loop processes keyboard inputs, updates game objects, and renders frames.
- **Object Modeling:** The game leverages an object-oriented design in Haskell with a hierarchy of game objects and components, similar to models found in classic C++/SDL projects.
- **Customization:** You can modify the window size, paddle speed, ball behavior, and even the color scheme to create a unique Pong experience.

This project draws inspiration from the [TP1: Pong project description](https://lucasnfe.github.io/dcc192-2025-1/avaliacoes/tp1-pong) used in the UFMG DCC192 course, adapting its concepts to a Haskell/SDL environment.

## Features

- **Dynamic Game Loop:** Maintains a smooth gameplay experience with a dynamically adjusted framerate.
- **Game Object Hierarchy:** Implements Actors and Components for game entities like the Ball and Paddle.
- **Customizable Elements:** Easily adjust game settings such as window dimensions, paddle and ball speeds, and color schemes.
- **SDL2 Rendering:** Uses SDL2 for graphics and input, taking advantage of Haskell's type safety and expressive syntax.

## Requirements

- [SDL2](https://www.libsdl.org/) library, with `sdl2`, `sdl2-ttf`, `sdl2-mixer` and `sdl2-image`.
- `GHC` and `Cabal`. I recommend installing [ghcup](https://www.haskell.org/ghcup/). Then you just need to set the version with `ghcup set ghc 9.4.8` and `cabal update`.

## Installation

Clone the repository and navigate into the project directory:

```
git clone https://github.com/<YOUR_GITHUB_USERNAME>/haskell-pong.git
cd haskell-pong
```

### Building with Cabal

```
cabal update
cabal build
```

## Running the Game

After building the project, launch the game with one of the following commands:

  ```
  cabal run
  ```

A window will open displaying the Pong game. Use the keyboard (e.g., arrow keys) to control your paddle and keep the ball in play.

## Project Structure

- **app/**
  - `Main.hs` – The entry point of the game.
- **src/**
  - `Game.hs` – Handles SDL initialization, the main game loop, and overall game management.
  - `Actors` – Defines the game objects (e.g., Ball and Paddle) and their behaviors.
  - `Components` – Contains modules for rendering and input handling components.
- **haskell-pong.cabal** – Project configuration and dependency management.
- **README.md** – This documentation file.

## License

This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for more details.

