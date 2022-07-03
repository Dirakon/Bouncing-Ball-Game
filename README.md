# Bouncing-Ball-Game

### Project description

![Gameplay demo](https://user-images.githubusercontent.com/43912367/177056990-d6d74ae5-70e9-4bd2-9bb9-b523b5ca54b9.gif)

Game:
This is a playable version with cannon, walls, and ball collision physics. There also is a trajectory of cannon shoot.
The player can restart the level and switch the level to the next if they win. The number of balls in a game is limited. If a player reaches the end of created levels, they can create a new level and play it. Sounds and music accompany the gameplay!

Map editor:
The process of level creation is simple because the game has a built-in map editor. The map editor allows the player to place a ball and remove it using the right and left mouse buttons correspondingly. Moreover, the map editor provides the player the ability to change the enemy ball radius using the mouse wheel. Player also can change background picture/music. 


### How to run the game
For installation, you will need [Cabal tool](https://www.haskell.org/cabal/), [SDL 2](https://www.libsdl.org/download-2.0.php), [SDL_mixer 2](https://libsdl.org/projects/SDL_mixer/)

Alternatively, if you have a problem with installing either SDL or SDL_mixer, you could manually disable sounds:<br>
&nbsp;&nbsp;&nbsp;&nbsp;You will need to go to `*.cabal` file in the project, and set `sound` flag to False as default

Do the following:<br>
<pre>git clone https://github.com/Dirakon/Bouncing-Ball-Game.git<br>
cd Bouncing-Ball-Game<br>
cabal build<br>
cabal run</pre>

To play, use mouse to set ball trajectory and left-click to shoot.
In editor mode, use mouse wheel to change size of the ball, right-click to place the ball, left-click to remove the ball.

### Authors

Dmitry Bannikov

[<img src="https://badges.aleen42.com/src/telegram.svg">](https://t.me/Dirak0n)

Kirill Korolev

[<img src="https://badges.aleen42.com/src/telegram.svg">](https://t.me/zaqbez39me)
  

### Music used

<a href = "http://dig.ccmixter.org/files/sparky/65096">Toi encore (encore) by sparky (c) copyright 2022 Licensed under a Creative Commons Attribution Noncommercial license.</a> <br/>
<a href = "http://dig.ccmixter.org/files/septahelix/33947">Tocharian C by septahelix (c) copyright 2014 Licensed under a Creative Commons Attribution (3.0) license.</a> <br/>
<a href = "http://dig.ccmixter.org/files/JeffSpeed68/58628">The Vendetta by Stefan Kartenberg (c) copyright 2018 Licensed under a Creative Commons Attribution (3.0) license. Ft: Apoxode</a> <br/>

### Acknowledgements
The usage of SDL2_mixer is based on [this example](https://gitlab.homotopic.tech/haskell/sdl2-mixer/-/blob/master/examples/Basic/Main.hs). <br>
The usage of Gloss is based on [this post](https://andrew.gibiansky.com/blog/haskell/haskell-gloss/). <br>
The usage of C pre-processor is based on [this thread](https://stackoverflow.com/questions/6361846/where-can-i-learn-about-ifdef). <br>
Segment-circle intersection algorithm is based on [this code example](https://rosettacode.org/wiki/Line_circle_intersection#Haskell). <br>

