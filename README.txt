# PETAL GARDEN

A small recreation of an old Microsoft Game Studio Game on Windows Phone.

I used to play Flowerz quite a lot, but it doesn't load on my old phone
anymore. This is a basic recreation in Elm based on images and videos on
the internet.

Play
----

Open the index.html file and play. You can rebuild the elm file if you
want to.

Saves and import
----------------

The game uses localStorage for saved games.

Source layout
-------------
src/Engine.elm: pure rules, typed cells/pieces/colours, seeded piece generation,
               match/cascade resolution, stone layout, v1 JSON codecs.
src/Main.elm:   Elm Architecture model/update/view, SVG artwork, keyboard/touch
               interaction, accessible dialogs and notifications.
interop.js:    browser-only localStorage, crypto startup seed, native dialog
               opening and Web Audio through Elm ports. No game rules.


Adjusting the rules
------------------
Change stoneTarget/stoneOrder, batchSize, doubleChance or pieceGenerator in
Engine.elm. Main.elm sends action, current pop number and level-completion audio
events to interop.js; all sound synthesis lives in that JavaScript file.
play and skip return Rejected, Selecting or Played; only Played consumes a
piece. resolve applies each matching wave simultaneously, with no gravity.
finish resolves matches, advances the level, adjusts stones, then refills
the queue. Existing flowers are never overwritten by stones.

Building
--------
This project targets Elm 0.19.2, but should work with other elm version in
the series.
From this directory:

    make

Or:

    elm make src/Main.elm --optimize --output=elm.js
