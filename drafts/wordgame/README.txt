This is a helper for a small word game I created in November 2020.

The script, when launched, prints 5 random combinations of letters.
The players then in turn think up the words containing one or more
of those combinations. The more combinations are present in the word the better.

The players keep the score separately for each of combinations.
So the score table might look like this:

       | Alice | Bob |
       ---------------
FO     | 3     |  2  |
BA     | 2     |  2  |
XY     | 0     |  1  |
YO     | 3     |  3  |
ER     | 1     |  2  |

Whoever is best at most of the combinations (in this case, Bob), wins the game.

The script requires the OpenCorpora dictionary present in the same directory
under the name "dict.opcorpora.txt".
It can be downloaded here: http://opencorpora.org/dict.php
