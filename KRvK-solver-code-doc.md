# KRvK-solver Code Documentation
## What is KRvK-solver
KRvK-solver is a program, that plays king and rook, which are the white pieces, in a **K**ing and **R**ook **V**ersus **K**ing chess position while the user plays the opposing black pieces (or, in this situation, a piece).
## Algorithm
This program uses an algorithm, that is commonly used by humans when playing chess. Since this is the case, it might be easier to understand from an explanation video: https://www.youtube.com/watch?v=VOGeEZ7iYSg. Here are the steps:
1. Place the rook in a line or row, so that it separates the kings. If this is not possible in this turn, move the rook as far from the white king as possible so that they don't share a line or row and try again. This will always work on the second try, because the kings always have a line or a row between them in some direction, therefore the only way we couldn't have separated them in the first move is, that the rook was behind the white king and the black king was somewhere in front of the white king and we can definitely find this separating line or row upon moving away.
2. Repeat until checkmate:
    1. If the rook is attacked, move it to the other side.
    2. If the rook can safely cut off the black king more, it moves closer to the black king.
    3. If the kings are in an L shape, the rook does a waiting move, forcing the black king to either take opposition or move closer to the edge.
    4. If not, the white king tries to push the black king to the edge by trying to create an L shape between the kings. This makes it so the black king has to either run in one direction or take the opposition of the white king, after which the rook can check it and cut it off more.

This algorithm always leads to a checkmate as we continue to cut off black kings space, it will arrive at the edge, having no place to move back anymore and after white forces king opposition again, the rook delivers the checkmate.
## Some Key Functions
- main: Initializes the game by prompting for piece positions and starting the game loop if the board is valid.
- gameLoop: Alternates turns between the computer and the player until the Black King is checkmated.
- playerMove: Updates the board based on the player's move if it's legal. Reprompts if it's illegal.
- computerMove: Determines the computer's move to progress towards checkmate.
- progress: Determines the computer's moves, if the rook is separating the kings.