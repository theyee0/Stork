# Stork
A procedurally generated fantasy game, aimed to explore traditional story generation techniques to approach an experience like Zork.

# Usage
## Demo
A short demo showing the basic interface:

https://github.com/user-attachments/assets/aaf94dbb-0232-4ef8-bad2-d03ac747863c

## Building and Running
You can run with the following command, assuming you have `sbcl` installed:
```
sbcl --script run.lisp
```

Alternatively, you can run the program from the REPL. This also requires a lisp compiler to be installed. I'd suggest using SBCL, which you can run with
```
sbcl
``
in the command line. It should then show a prompt with an asterisk, into which you can then type commands. If you get a prompt with a number and a bracket like `0]` or `1]`, you are in the debugger. You can type the number corresponding to "ABORT" or similar, and it should put you back into the normal asterisk prompt.

The file uses [asdf](https://asdf.common-lisp.dev/) for building. You can enter the following commands into a REPL to build, assuming you have asdf installed already:
```
(require 'asdf)
(load "stork.asd")
(asdf:load-system "stork")
```

Then, you can run the game loop through
```
(stork:main)
```

Currently, the following commands are implemented:
- `wait` - Lets a turn pass without you doing anything
- `move` - Moves in a cardinal direction (north, east, south, west)
Technically speaking, `look`, `use`, `search`, `attack` also work, but since the story and characters have not been written yet, there is nothing to see, use, search, or attack.

## To-Do:
- [x] Create unified system for callback functions
- [x] Define data types for rooms, characters, etc.
- [x] Determine mechanisms for allowing properties to change behaviors (callback function?)
- [ ] Write sample tables for storytelling
- [ ] Select map generation algorithm
- [x] Write parser
- [x] Set up build systems

## Vision and Plan
The game runs in "ticks," or turns. Between ticks, the player will have time to think without the program responding. The number of events between each tick can be configured manually, and more events per tick will necessarily make a harder game for you!

The program is structured into a few primary components:
1. `stork.lisp` is the file that will create the final executive
2. `event.lisp` takes the game state and the event queue, then selects an event from that.
3. `map.lisp` generates the map of the game
3. `entity.lisp` generates a character's backstory and traits. Depends on `event.lisp`
4. `history.lisp` generates lore
5. `parse.lisp` takes user input and determines what kind of command the user made
6. `logic.lisp` defines conditions and will hopefully serve to check for logical inconsistencies

## Motivation
I decided that developing in C, my preferred language, would be too difficult simply due to the minimalism of the language. Therefore, I decided to explore the opportunity to write this program in Common Lisp, which is another language that interests me.

I also wanted to explore procedural storytelling without the use of artificial intelligence - the aim is to one day extend this program with a richer set of story rules and to include some concepts from computational semantics to approach the level of immersiveness that a human storyteller (or an LLM) can provide. I was inspired in particular by dwarf fortress's procedural storytelling.
