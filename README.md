# Bouncing-Ball-Game

![image](https://user-images.githubusercontent.com/50139678/174792711-d5012e5d-04a1-4528-9230-ea11b3aa06ce.png)

### Project description

Game:
This MVP is a playable version with cannon, walls, and ball collision physics. There also is a future trajectory of cannon shoot.
The player can restart the level and switch the level to the next if they win. The number of balls in a game is limited. If a player reaches the end of created levels, they can create a new level and play it.

Map editor:
The process of level creation is simple because the game has a built-in map editor. The map editor allows the player to place a ball and remove it using the right and left mouse buttons correspondingly. Moreover, the map editor provides the player the ability to change the enemy ball radius using the mouse wheel.


### How to run the game
For installation, you will need [Cabal tool](https://www.haskell.org/cabal/), [SDL 2](https://www.libsdl.org/download-2.0.php), [SDL_mixer 2](https://libsdl.org/projects/SDL_mixer/)

Alternatively, if you have a problem with installing either SDL or SDL_mixer, you could manually disable sounds:<br>
&nbsp;&nbsp;&nbsp;&nbsp;You will need to go to `*.cabal` file in the project, and set `sounds` flag to False as default

Then, do<br>
<pre>git clone https://github.com/Dirakon/Bouncing-Ball-Game.git<br>
cd Bouncing-Ball-Game<br>
cabal build<br>
cabal run</pre>.

To play, use mouse to set ball trajectory and left-click to shoot.
In editor mode, use mouse wheel to change size of the ball, right-click to place the ball, left-click to remove the ball.


### Acknowledgements
The usage of SDL2_mixer is based on [this example](https://gitlab.homotopic.tech/haskell/sdl2-mixer/-/blob/master/examples/Basic/Main.hs).
The usage of Gloss is based on [this post](https://andrew.gibiansky.com/blog/haskell/haskell-gloss/).
The usage of C pre-processor is based on [this thread](https://stackoverflow.com/questions/6361846/where-can-i-learn-about-ifdef)
