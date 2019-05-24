#!/bin/bash

# Fri Sep 28 14:48:11 CDT 2018 : bug fix heros.txt file out of order

# Thu Sep 27 15:09:26 CDT 2018 : bug fixes from original version

T=0                             # global test number

((T++))
tnames[T]="show-quit"
read  -r -d '' input[$T] <<"ENDIN"
help
quit
ENDIN
#
read  -r -d '' output[$T] <<"ENDOUT"
(default.txt)> help
MULTI MANAGER
Maintains multiple sorted lists of unique elements..

--PROGRAM COMMANDS--:
  help           : print this help message
  quit           : quit the program

--CURRENT LIST COMMANDS--
The following commands modify the current list
  show           : print the current list to the screen
  clear          : set the list to empty, preserves undo history
  add <elem>     : add elem to the list
  remove <elem>  : remove elem from list
  mergein <file> : load the sorted list in the named file and merge with current list (undoable)
  save           : save the current list using the name of the list as the save file
  saveas <file>  : save the current list to the given file name; keeps the list name the same
  undo           : undo the last operation restoring list to a previous state
  redo           : redo the last undone operation restoring list to a previous state

--LIST MANAGEMENT COMMANDS--
The following commands will fail if a list name is already in use (new/open) or no present (close/edit/merge)
  lists          : prints the lists that are currently open
  edit <list>    : set the named list to the current list
  new <list>     : create a new empty list and switch to it
  open <file>    : create a new list named after the file specified; load the contents of the file into it and switch to it
  close <list>   : close the list with given name and remove it from the open documents; cannot close the current list
  merge <list>   : merge the named list contents into the current list

--BULK OPERATIONS--
The following commands act upon all open lists
  showall        : print all lists labelled with their list name
  saveall        : save all open lists; use filenames identical the list names (not undoable)
  addall <elem>  : add elem to all open lists; each list can undo this individually
  mergeall       : merge the contents of all lists into the current list; undoable
(default.txt)> quit

Lists multi-managed!
ENDOUT

((T++))
tnames[T]="add-remove-1-list"
read  -r -d '' input[$T] <<"ENDIN"
add Korra
add Mako
add Bolin
add Asami
add Tenzin
add Meelo
show
add Bolin
add Tenzin
show
remove Mako
remove Tenzin
show
ENDIN
#
read  -r -d '' output[$T] <<"ENDOUT"
(default.txt)> add Korra
(default.txt)> add Mako
(default.txt)> add Bolin
(default.txt)> add Asami
(default.txt)> add Tenzin
(default.txt)> add Meelo
(default.txt)> show
--BEG LIST--
Asami
Bolin
Korra
Mako
Meelo
Tenzin
--END LIST--
(default.txt)> add Bolin
(default.txt)> add Tenzin
(default.txt)> show
--BEG LIST--
Asami
Bolin
Korra
Mako
Meelo
Tenzin
--END LIST--
(default.txt)> remove Mako
(default.txt)> remove Tenzin
(default.txt)> show
--BEG LIST--
Asami
Bolin
Korra
Meelo
--END LIST--
(default.txt)> 
Lists multi-managed!
ENDOUT

((T++))
tnames[T]="1-list-all-cmds"
read  -r -d '' input[$T] <<"ENDIN"
add Korra
add Mako
add Bolin
add Asami
add Tenzin
add Meelo
undo
undo
show
redo
show
add Jinora
add Bumi
show
saveas test-data/first.tmp
clear
show
undo
show
load test-data/villains.txt
show
load test-data/first.tmp
show
undo
show
mergein test-data/heros.txt
show
remove Amon
remove Kuvira
show
undo
undo
undo
show
add Zaheer
redo
show
quit
ENDIN
#
read  -r -d '' output[$T] <<"ENDOUT"
(default.txt)> add Korra
(default.txt)> add Mako
(default.txt)> add Bolin
(default.txt)> add Asami
(default.txt)> add Tenzin
(default.txt)> add Meelo
(default.txt)> undo
(default.txt)> undo
(default.txt)> show
--BEG LIST--
Asami
Bolin
Korra
Mako
--END LIST--
(default.txt)> redo
(default.txt)> show
--BEG LIST--
Asami
Bolin
Korra
Mako
Tenzin
--END LIST--
(default.txt)> add Jinora
(default.txt)> add Bumi
(default.txt)> show
--BEG LIST--
Asami
Bolin
Bumi
Jinora
Korra
Mako
Tenzin
--END LIST--
(default.txt)> saveas test-data/first.tmp
(default.txt)> clear
(default.txt)> show
--BEG LIST--
--END LIST--
(default.txt)> undo
(default.txt)> show
--BEG LIST--
Asami
Bolin
Bumi
Jinora
Korra
Mako
Tenzin
--END LIST--
(default.txt)> load test-data/villains.txt
(default.txt)> show
--BEG LIST--
Amon
Hiroshi
Kuvira
Ming-Hua
P-li
Unalaq
Zaheer
--END LIST--
(default.txt)> load test-data/first.tmp
(default.txt)> show
--BEG LIST--
Asami
Bolin
Bumi
Jinora
Korra
Mako
Tenzin
--END LIST--
(default.txt)> undo
(default.txt)> show
--BEG LIST--
Amon
Hiroshi
Kuvira
Ming-Hua
P-li
Unalaq
Zaheer
--END LIST--
(default.txt)> mergein test-data/heros.txt
(default.txt)> show
--BEG LIST--
Amon
Asami
Bolin
Bumi
Hiroshi
Jinora
Korra
Kuvira
Kya
Mako
Ming-Hua
P-li
Tenzin
Unalaq
Zaheer
--END LIST--
(default.txt)> remove Amon
(default.txt)> remove Kuvira
(default.txt)> show
--BEG LIST--
Asami
Bolin
Bumi
Hiroshi
Jinora
Korra
Kya
Mako
Ming-Hua
P-li
Tenzin
Unalaq
Zaheer
--END LIST--
(default.txt)> undo
(default.txt)> undo
(default.txt)> undo
(default.txt)> show
--BEG LIST--
Amon
Hiroshi
Kuvira
Ming-Hua
P-li
Unalaq
Zaheer
--END LIST--
(default.txt)> add Zaheer
(default.txt)> redo
WARNING: redo list empty, no changes made
(default.txt)> show
--BEG LIST--
Amon
Hiroshi
Kuvira
Ming-Hua
P-li
Unalaq
Zaheer
--END LIST--
(default.txt)> quit

Lists multi-managed!
ENDOUT

((T++))
tnames[T]="2-lists-add"
read  -r -d '' input[$T] <<"ENDIN"
add Korra
add Bolin
show
new other.txt
show
add Asami
add Mako
show
edit default.txt
show
lists
ENDIN
#
read  -r -d '' output[$T] <<"ENDOUT"
(default.txt)> add Korra
(default.txt)> add Bolin
(default.txt)> show
--BEG LIST--
Bolin
Korra
--END LIST--
(default.txt)> new other.txt
(other.txt)> show
--BEG LIST--
--END LIST--
(other.txt)> add Asami
(other.txt)> add Mako
(other.txt)> show
--BEG LIST--
Asami
Mako
--END LIST--
(other.txt)> edit default.txt
(default.txt)> show
--BEG LIST--
Bolin
Korra
--END LIST--
(default.txt)> lists
2 docs
- other.txt
- default.txt
(default.txt)> 
Lists multi-managed!
ENDOUT

((T++))
tnames[T]="2-lists-rm-undo"
read  -r -d '' input[$T] <<"ENDIN"
add Korra
add Bolin
add Tenzin
show
new other.txt
show
add Asami
add Mako
show
edit default.txt
remove Korra
show
edit other.txt
add Mako
add Lin
remove Asami
show
edit default.txt
undo
show
edit other.txt
undo
show
lists
ENDIN
#
read  -r -d '' output[$T] <<"ENDOUT"
(default.txt)> add Korra
(default.txt)> add Bolin
(default.txt)> add Tenzin
(default.txt)> show
--BEG LIST--
Bolin
Korra
Tenzin
--END LIST--
(default.txt)> new other.txt
(other.txt)> show
--BEG LIST--
--END LIST--
(other.txt)> add Asami
(other.txt)> add Mako
(other.txt)> show
--BEG LIST--
Asami
Mako
--END LIST--
(other.txt)> edit default.txt
(default.txt)> remove Korra
(default.txt)> show
--BEG LIST--
Bolin
Tenzin
--END LIST--
(default.txt)> edit other.txt
(other.txt)> add Mako
(other.txt)> add Lin
(other.txt)> remove Asami
(other.txt)> show
--BEG LIST--
Lin
Mako
--END LIST--
(other.txt)> edit default.txt
(default.txt)> undo
(default.txt)> show
--BEG LIST--
Bolin
Korra
Tenzin
--END LIST--
(default.txt)> edit other.txt
(other.txt)> undo
(other.txt)> show
--BEG LIST--
Asami
Lin
Mako
--END LIST--
(other.txt)> lists
2 docs
- other.txt
- default.txt
(other.txt)> 
Lists multi-managed!
ENDOUT

((T++))
tnames[T]="3-lists-basics"
read  -r -d '' input[$T] <<"ENDIN"
add Korra
add Bolin
add Tenzin
show
new second.txt
show
add Asami
add Mako
show
new third.txt
lists
show
add Amon
add Korra
add Jinora
show
edit default.txt
show
add Mako
add Lin
remove Bolin
show
edit third.txt
show
undo
show
edit second.txt
show
undo
show
edit default.txt
edit second.txt
redo
show
lists
ENDIN
#
read  -r -d '' output[$T] <<"ENDOUT"
(default.txt)> add Korra
(default.txt)> add Bolin
(default.txt)> add Tenzin
(default.txt)> show
--BEG LIST--
Bolin
Korra
Tenzin
--END LIST--
(default.txt)> new second.txt
(second.txt)> show
--BEG LIST--
--END LIST--
(second.txt)> add Asami
(second.txt)> add Mako
(second.txt)> show
--BEG LIST--
Asami
Mako
--END LIST--
(second.txt)> new third.txt
(third.txt)> lists
3 docs
- third.txt
- second.txt
- default.txt
(third.txt)> show
--BEG LIST--
--END LIST--
(third.txt)> add Amon
(third.txt)> add Korra
(third.txt)> add Jinora
(third.txt)> show
--BEG LIST--
Amon
Jinora
Korra
--END LIST--
(third.txt)> edit default.txt
(default.txt)> show
--BEG LIST--
Bolin
Korra
Tenzin
--END LIST--
(default.txt)> add Mako
(default.txt)> add Lin
(default.txt)> remove Bolin
(default.txt)> show
--BEG LIST--
Korra
Lin
Mako
Tenzin
--END LIST--
(default.txt)> edit third.txt
(third.txt)> show
--BEG LIST--
Amon
Jinora
Korra
--END LIST--
(third.txt)> undo
(third.txt)> show
--BEG LIST--
Amon
Korra
--END LIST--
(third.txt)> edit second.txt
(second.txt)> show
--BEG LIST--
Asami
Mako
--END LIST--
(second.txt)> undo
(second.txt)> show
--BEG LIST--
Asami
--END LIST--
(second.txt)> edit default.txt
(default.txt)> edit second.txt
(second.txt)> redo
(second.txt)> show
--BEG LIST--
Asami
Mako
--END LIST--
(second.txt)> lists
3 docs
- third.txt
- second.txt
- default.txt
(second.txt)> 
Lists multi-managed!
ENDOUT

((T++))
tnames[T]="open-close-save"
read  -r -d '' input[$T] <<"ENDIN"
add Korra
add Bolin
show
new second.txt
add Bumi
add Kya
add Jinora
show
lists
close default.txt
lists
new third.txt
new fourth.txt
new fifth.txt
lists
close fourth.txt
lists
edit fifth.txt
add Tenzin
add Bolin
add Pema
show
edit second.txt
show
edit fifth.txt
show
new test-data/sixth.tmp
add Korra
add Mako
add Bolin
save
new seventh.txt
open test-data/sixth.tmp
lists
close test-data/sixth.tmp
lists
open test-data/sixth.tmp
show
edit seventh.txt
load test-data/sixth.tmp
ENDIN
#
read  -r -d '' output[$T] <<"ENDOUT"
(default.txt)> add Korra
(default.txt)> add Bolin
(default.txt)> show
--BEG LIST--
Bolin
Korra
--END LIST--
(default.txt)> new second.txt
(second.txt)> add Bumi
(second.txt)> add Kya
(second.txt)> add Jinora
(second.txt)> show
--BEG LIST--
Bumi
Jinora
Kya
--END LIST--
(second.txt)> lists
2 docs
- second.txt
- default.txt
(second.txt)> close default.txt
(second.txt)> lists
1 docs
- second.txt
(second.txt)> new third.txt
(third.txt)> new fourth.txt
(fourth.txt)> new fifth.txt
(fifth.txt)> lists
4 docs
- fifth.txt
- fourth.txt
- third.txt
- second.txt
(fifth.txt)> close fourth.txt
(fifth.txt)> lists
3 docs
- fifth.txt
- third.txt
- second.txt
(fifth.txt)> edit fifth.txt
(fifth.txt)> add Tenzin
(fifth.txt)> add Bolin
(fifth.txt)> add Pema
(fifth.txt)> show
--BEG LIST--
Bolin
Pema
Tenzin
--END LIST--
(fifth.txt)> edit second.txt
(second.txt)> show
--BEG LIST--
Bumi
Jinora
Kya
--END LIST--
(second.txt)> edit fifth.txt
(fifth.txt)> show
--BEG LIST--
Bolin
Pema
Tenzin
--END LIST--
(fifth.txt)> new test-data/sixth.tmp
(test-data/sixth.tmp)> add Korra
(test-data/sixth.tmp)> add Mako
(test-data/sixth.tmp)> add Bolin
(test-data/sixth.tmp)> save
(test-data/sixth.tmp)> new seventh.txt
(seventh.txt)> open test-data/sixth.tmp
ERROR: list 'test-data/sixth.tmp' already exists
(seventh.txt)> lists
5 docs
- seventh.txt
- test-data/sixth.tmp
- fifth.txt
- third.txt
- second.txt
(seventh.txt)> close test-data/sixth.tmp
(seventh.txt)> lists
4 docs
- seventh.txt
- fifth.txt
- third.txt
- second.txt
(seventh.txt)> open test-data/sixth.tmp
(test-data/sixth.tmp)> show
--BEG LIST--
Bolin
Korra
Mako
--END LIST--
(test-data/sixth.tmp)> edit seventh.txt
(seventh.txt)> load test-data/sixth.tmp
(seventh.txt)> 
Lists multi-managed!
ENDOUT


((T++))
tnames[T]="mult-close-curr"
read  -r -d '' input[$T] <<"ENDIN"
add Korra
add Bolin
show
close default.txt
lists
new second.txt
load test-data/heros.txt
show
lists
close second.txt
edit default.txt
show
lists
new third.txt
close third.txt
lists
close second.txt
lists
edit second.txt
edit default.txt
show
ENDIN
#
read  -r -d '' output[$T] <<"ENDOUT"
(default.txt)> add Korra
(default.txt)> add Bolin
(default.txt)> show
--BEG LIST--
Bolin
Korra
--END LIST--
(default.txt)> close default.txt
ERROR: cannot close the current list
(default.txt)> lists
1 docs
- default.txt
(default.txt)> new second.txt
(second.txt)> load test-data/heros.txt
(second.txt)> show
--BEG LIST--
Asami
Bolin
Bumi
Jinora
Korra
Kya
Mako
Tenzin
--END LIST--
(second.txt)> lists
2 docs
- second.txt
- default.txt
(second.txt)> close second.txt
ERROR: cannot close the current list
(second.txt)> edit default.txt
(default.txt)> show
--BEG LIST--
Bolin
Korra
--END LIST--
(default.txt)> lists
2 docs
- second.txt
- default.txt
(default.txt)> new third.txt
(third.txt)> close third.txt
ERROR: cannot close the current list
(third.txt)> lists
3 docs
- third.txt
- second.txt
- default.txt
(third.txt)> close second.txt
(third.txt)> lists
2 docs
- third.txt
- default.txt
(third.txt)> edit second.txt
ERROR: list 'second.txt' does not exist
(third.txt)> edit default.txt
(default.txt)> show
--BEG LIST--
Bolin
Korra
--END LIST--
(default.txt)> 
Lists multi-managed!
ENDOUT

((T++))
tnames[T]="mult-edit-nothere"
read  -r -d '' input[$T] <<"ENDIN"
add Korra
add Bolin
show
edit nope.txt
lists
new second.txt
load test-data/heros.txt
show
lists
edit dflt.txt
new third.txt
close default.txt
lists
edit default.txt
edit second.txt
show
lists
close third.txt
lists
edit third.txt
lists
ENDIN
#
read  -r -d '' output[$T] <<"ENDOUT"
(default.txt)> add Korra
(default.txt)> add Bolin
(default.txt)> show
--BEG LIST--
Bolin
Korra
--END LIST--
(default.txt)> edit nope.txt
ERROR: list 'nope.txt' does not exist
(default.txt)> lists
1 docs
- default.txt
(default.txt)> new second.txt
(second.txt)> load test-data/heros.txt
(second.txt)> show
--BEG LIST--
Asami
Bolin
Bumi
Jinora
Korra
Kya
Mako
Tenzin
--END LIST--
(second.txt)> lists
2 docs
- second.txt
- default.txt
(second.txt)> edit dflt.txt
ERROR: list 'dflt.txt' does not exist
(second.txt)> new third.txt
(third.txt)> close default.txt
(third.txt)> lists
2 docs
- third.txt
- second.txt
(third.txt)> edit default.txt
ERROR: list 'default.txt' does not exist
(third.txt)> edit second.txt
(second.txt)> show
--BEG LIST--
Asami
Bolin
Bumi
Jinora
Korra
Kya
Mako
Tenzin
--END LIST--
(second.txt)> lists
2 docs
- third.txt
- second.txt
(second.txt)> close third.txt
(second.txt)> lists
1 docs
- second.txt
(second.txt)> edit third.txt
ERROR: list 'third.txt' does not exist
(second.txt)> lists
1 docs
- second.txt
(second.txt)> 
Lists multi-managed!
ENDOUT

((T++))
tnames[T]="mult-open"
read  -r -d '' input[$T] <<"ENDIN"
add Meelo
add Pema
show
open test-data/heros.txt
show
lists
open test-data/villains.txt
show
lists
undo
redo
edit default.txt
show
undo
show
edit test-data/heros.txt
show
new second.txt
load test-data/heros.txt
show
undo
show
lists
ENDIN
#
read  -r -d '' output[$T] <<"ENDOUT"
(default.txt)> add Meelo
(default.txt)> add Pema
(default.txt)> show
--BEG LIST--
Meelo
Pema
--END LIST--
(default.txt)> open test-data/heros.txt
(test-data/heros.txt)> show
--BEG LIST--
Asami
Bolin
Bumi
Jinora
Korra
Kya
Mako
Tenzin
--END LIST--
(test-data/heros.txt)> lists
2 docs
- test-data/heros.txt
- default.txt
(test-data/heros.txt)> open test-data/villains.txt
(test-data/villains.txt)> show
--BEG LIST--
Amon
Hiroshi
Kuvira
Ming-Hua
P-li
Unalaq
Zaheer
--END LIST--
(test-data/villains.txt)> lists
3 docs
- test-data/villains.txt
- test-data/heros.txt
- default.txt
(test-data/villains.txt)> undo
WARNING: undo list empty, no changes made
(test-data/villains.txt)> redo
WARNING: redo list empty, no changes made
(test-data/villains.txt)> edit default.txt
(default.txt)> show
--BEG LIST--
Meelo
Pema
--END LIST--
(default.txt)> undo
(default.txt)> show
--BEG LIST--
Meelo
--END LIST--
(default.txt)> edit test-data/heros.txt
(test-data/heros.txt)> show
--BEG LIST--
Asami
Bolin
Bumi
Jinora
Korra
Kya
Mako
Tenzin
--END LIST--
(test-data/heros.txt)> new second.txt
(second.txt)> load test-data/heros.txt
(second.txt)> show
--BEG LIST--
Asami
Bolin
Bumi
Jinora
Korra
Kya
Mako
Tenzin
--END LIST--
(second.txt)> undo
(second.txt)> show
--BEG LIST--
--END LIST--
(second.txt)> lists
4 docs
- second.txt
- test-data/villains.txt
- test-data/heros.txt
- default.txt
(second.txt)> 
Lists multi-managed!
ENDOUT

((T++))
tnames[T]="mult-merge"
read  -r -d '' input[$T] <<"ENDIN"
add Korra
add Bolin
add Mako
add Asami
show
new second.txt
show
merge default.txt
show
new third.txt
add Bolin
add Tenzin
add Bumi
add Mako
show
merge default.txt
show
undo
show
redo
show
undo
open test-data/villains.txt
show
edit default.txt
merge test-data/villains.txt
show
undo
show
redo 
show
ENDIN
#
read  -r -d '' output[$T] <<"ENDOUT"
(default.txt)> add Korra
(default.txt)> add Bolin
(default.txt)> add Mako
(default.txt)> add Asami
(default.txt)> show
--BEG LIST--
Asami
Bolin
Korra
Mako
--END LIST--
(default.txt)> new second.txt
(second.txt)> show
--BEG LIST--
--END LIST--
(second.txt)> merge default.txt
(second.txt)> show
--BEG LIST--
Asami
Bolin
Korra
Mako
--END LIST--
(second.txt)> new third.txt
(third.txt)> add Bolin
(third.txt)> add Tenzin
(third.txt)> add Bumi
(third.txt)> add Mako
(third.txt)> show
--BEG LIST--
Bolin
Bumi
Mako
Tenzin
--END LIST--
(third.txt)> merge default.txt
(third.txt)> show
--BEG LIST--
Asami
Bolin
Bumi
Korra
Mako
Tenzin
--END LIST--
(third.txt)> undo
(third.txt)> show
--BEG LIST--
Bolin
Bumi
Mako
Tenzin
--END LIST--
(third.txt)> redo
(third.txt)> show
--BEG LIST--
Asami
Bolin
Bumi
Korra
Mako
Tenzin
--END LIST--
(third.txt)> undo
(third.txt)> open test-data/villains.txt
(test-data/villains.txt)> show
--BEG LIST--
Amon
Hiroshi
Kuvira
Ming-Hua
P-li
Unalaq
Zaheer
--END LIST--
(test-data/villains.txt)> edit default.txt
(default.txt)> merge test-data/villains.txt
(default.txt)> show
--BEG LIST--
Amon
Asami
Bolin
Hiroshi
Korra
Kuvira
Mako
Ming-Hua
P-li
Unalaq
Zaheer
--END LIST--
(default.txt)> undo
(default.txt)> show
--BEG LIST--
Asami
Bolin
Korra
Mako
--END LIST--
(default.txt)> redo 
(default.txt)> show
--BEG LIST--
Amon
Asami
Bolin
Hiroshi
Korra
Kuvira
Mako
Ming-Hua
P-li
Unalaq
Zaheer
--END LIST--
(default.txt)> 
Lists multi-managed!
ENDOUT

((T++))
tnames[T]="bulk-showsave"
read  -r -d '' input[$T] <<"ENDIN"
new test-data/first.tmp
close default.txt
lists
add Korra
add Bolin
add Mako
add Asami
showall
new test-data/second.tmp
add Kuvira
add Amon
add Zaheer
add Unalaq
lists
showall
open test-data/heros.txt
open test-data/villains.txt
showall
new test-data/third.tmp
close test-data/heros.txt
close test-data/villains.txt
lists
load test-data/villains.txt
mergein test-data/heros.txt
showall
saveall
close test-data/first.tmp
close test-data/second.tmp
lists
open test-data/first.tmp
show
new fourth.txt
load test-data/second.tmp
show
showall
close test-data/third.tmp
open test-data/third.tmp
show
ENDIN
#
read  -r -d '' output[$T] <<"ENDOUT"
(default.txt)> new test-data/first.tmp
(test-data/first.tmp)> close default.txt
(test-data/first.tmp)> lists
1 docs
- test-data/first.tmp
(test-data/first.tmp)> add Korra
(test-data/first.tmp)> add Bolin
(test-data/first.tmp)> add Mako
(test-data/first.tmp)> add Asami
(test-data/first.tmp)> showall
--List test-data/first.tmp--
Asami
Bolin
Korra
Mako

(test-data/first.tmp)> new test-data/second.tmp
(test-data/second.tmp)> add Kuvira
(test-data/second.tmp)> add Amon
(test-data/second.tmp)> add Zaheer
(test-data/second.tmp)> add Unalaq
(test-data/second.tmp)> lists
2 docs
- test-data/second.tmp
- test-data/first.tmp
(test-data/second.tmp)> showall
--List test-data/second.tmp--
Amon
Kuvira
Unalaq
Zaheer

--List test-data/first.tmp--
Asami
Bolin
Korra
Mako

(test-data/second.tmp)> open test-data/heros.txt
(test-data/heros.txt)> open test-data/villains.txt
(test-data/villains.txt)> showall
--List test-data/villains.txt--
Amon
Hiroshi
Kuvira
Ming-Hua
P-li
Unalaq
Zaheer

--List test-data/heros.txt--
Asami
Bolin
Bumi
Jinora
Korra
Kya
Mako
Tenzin

--List test-data/second.tmp--
Amon
Kuvira
Unalaq
Zaheer

--List test-data/first.tmp--
Asami
Bolin
Korra
Mako

(test-data/villains.txt)> new test-data/third.tmp
(test-data/third.tmp)> close test-data/heros.txt
(test-data/third.tmp)> close test-data/villains.txt
(test-data/third.tmp)> lists
3 docs
- test-data/third.tmp
- test-data/second.tmp
- test-data/first.tmp
(test-data/third.tmp)> load test-data/villains.txt
(test-data/third.tmp)> mergein test-data/heros.txt
(test-data/third.tmp)> showall
--List test-data/third.tmp--
Amon
Asami
Bolin
Bumi
Hiroshi
Jinora
Korra
Kuvira
Kya
Mako
Ming-Hua
P-li
Tenzin
Unalaq
Zaheer

--List test-data/second.tmp--
Amon
Kuvira
Unalaq
Zaheer

--List test-data/first.tmp--
Asami
Bolin
Korra
Mako

(test-data/third.tmp)> saveall
(test-data/third.tmp)> close test-data/first.tmp
(test-data/third.tmp)> close test-data/second.tmp
(test-data/third.tmp)> lists
1 docs
- test-data/third.tmp
(test-data/third.tmp)> open test-data/first.tmp
(test-data/first.tmp)> show
--BEG LIST--
Asami
Bolin
Korra
Mako
--END LIST--
(test-data/first.tmp)> new fourth.txt
(fourth.txt)> load test-data/second.tmp
(fourth.txt)> show
--BEG LIST--
Amon
Kuvira
Unalaq
Zaheer
--END LIST--
(fourth.txt)> showall
--List fourth.txt--
Amon
Kuvira
Unalaq
Zaheer

--List test-data/first.tmp--
Asami
Bolin
Korra
Mako

--List test-data/third.tmp--
Amon
Asami
Bolin
Bumi
Hiroshi
Jinora
Korra
Kuvira
Kya
Mako
Ming-Hua
P-li
Tenzin
Unalaq
Zaheer

(fourth.txt)> close test-data/third.tmp
(fourth.txt)> open test-data/third.tmp
(test-data/third.tmp)> show
--BEG LIST--
Amon
Asami
Bolin
Bumi
Hiroshi
Jinora
Korra
Kuvira
Kya
Mako
Ming-Hua
P-li
Tenzin
Unalaq
Zaheer
--END LIST--
(test-data/third.tmp)> 
Lists multi-managed!
ENDOUT

((T++))
tnames[T]="bulk-addall"
read  -r -d '' input[$T] <<"ENDIN"
new second.txt
new third.txt
lists
showall
addall Korra
showall
undo
show
add Bolin
add Mako
add Asami
edit second.txt
add Tenzin
show
edit default.txt
undo
show
add Pema
add Bumi
showall
addall Mako
showall
addall Tenzin
showall
edit third.txt
show
undo
show
ENDIN
#
read  -r -d '' output[$T] <<"ENDOUT"
(default.txt)> new second.txt
(second.txt)> new third.txt
(third.txt)> lists
3 docs
- third.txt
- second.txt
- default.txt
(third.txt)> showall
--List third.txt--

--List second.txt--

--List default.txt--

(third.txt)> addall Korra
(third.txt)> showall
--List third.txt--
Korra

--List second.txt--
Korra

--List default.txt--
Korra

(third.txt)> undo
(third.txt)> show
--BEG LIST--
--END LIST--
(third.txt)> add Bolin
(third.txt)> add Mako
(third.txt)> add Asami
(third.txt)> edit second.txt
(second.txt)> add Tenzin
(second.txt)> show
--BEG LIST--
Korra
Tenzin
--END LIST--
(second.txt)> edit default.txt
(default.txt)> undo
(default.txt)> show
--BEG LIST--
--END LIST--
(default.txt)> add Pema
(default.txt)> add Bumi
(default.txt)> showall
--List third.txt--
Asami
Bolin
Mako

--List second.txt--
Korra
Tenzin

--List default.txt--
Bumi
Pema

(default.txt)> addall Mako
(default.txt)> showall
--List third.txt--
Asami
Bolin
Mako

--List second.txt--
Korra
Mako
Tenzin

--List default.txt--
Bumi
Mako
Pema

(default.txt)> addall Tenzin
(default.txt)> showall
--List third.txt--
Asami
Bolin
Mako
Tenzin

--List second.txt--
Korra
Mako
Tenzin

--List default.txt--
Bumi
Mako
Pema
Tenzin

(default.txt)> edit third.txt
(third.txt)> show
--BEG LIST--
Asami
Bolin
Mako
Tenzin
--END LIST--
(third.txt)> undo
(third.txt)> show
--BEG LIST--
Asami
Bolin
Mako
--END LIST--
(third.txt)> 
Lists multi-managed!
ENDOUT

((T++))
tnames[T]="bulk-mergeall"
read  -r -d '' input[$T] <<"ENDIN"
add Korra
new second.txt
add Mako
new third.txt
add Bolin
new fourth.txt
add Asami
showall
show
mergeall
show
undo
show
edit default.txt
mergeall
show
add Jinora
add Bumi
open test-data/villains.txt
showall
show
mergeall
show
undo
open test-data/heros.txt
showall
show
mergeall
show
new fifth.txt
close test-data/villains.txt
close test-data/heros.txt
edit second.txt
add Korra
add Hiroshi
edit fourth.txt
add Mako
new sixth.txt
showall
show
mergeall
show
ENDIN
#
read  -r -d '' output[$T] <<"ENDOUT"
(default.txt)> add Korra
(default.txt)> new second.txt
(second.txt)> add Mako
(second.txt)> new third.txt
(third.txt)> add Bolin
(third.txt)> new fourth.txt
(fourth.txt)> add Asami
(fourth.txt)> showall
--List fourth.txt--
Asami

--List third.txt--
Bolin

--List second.txt--
Mako

--List default.txt--
Korra

(fourth.txt)> show
--BEG LIST--
Asami
--END LIST--
(fourth.txt)> mergeall
(fourth.txt)> show
--BEG LIST--
Asami
Bolin
Korra
Mako
--END LIST--
(fourth.txt)> undo
(fourth.txt)> show
--BEG LIST--
Asami
--END LIST--
(fourth.txt)> edit default.txt
(default.txt)> mergeall
(default.txt)> show
--BEG LIST--
Asami
Bolin
Korra
Mako
--END LIST--
(default.txt)> add Jinora
(default.txt)> add Bumi
(default.txt)> open test-data/villains.txt
(test-data/villains.txt)> showall
--List test-data/villains.txt--
Amon
Hiroshi
Kuvira
Ming-Hua
P-li
Unalaq
Zaheer

--List fourth.txt--
Asami

--List third.txt--
Bolin

--List second.txt--
Mako

--List default.txt--
Asami
Bolin
Bumi
Jinora
Korra
Mako

(test-data/villains.txt)> show
--BEG LIST--
Amon
Hiroshi
Kuvira
Ming-Hua
P-li
Unalaq
Zaheer
--END LIST--
(test-data/villains.txt)> mergeall
(test-data/villains.txt)> show
--BEG LIST--
Amon
Asami
Bolin
Bumi
Hiroshi
Jinora
Korra
Kuvira
Mako
Ming-Hua
P-li
Unalaq
Zaheer
--END LIST--
(test-data/villains.txt)> undo
(test-data/villains.txt)> open test-data/heros.txt
(test-data/heros.txt)> showall
--List test-data/heros.txt--
Asami
Bolin
Bumi
Jinora
Korra
Kya
Mako
Tenzin

--List test-data/villains.txt--
Amon
Hiroshi
Kuvira
Ming-Hua
P-li
Unalaq
Zaheer

--List fourth.txt--
Asami

--List third.txt--
Bolin

--List second.txt--
Mako

--List default.txt--
Asami
Bolin
Bumi
Jinora
Korra
Mako

(test-data/heros.txt)> show
--BEG LIST--
Asami
Bolin
Bumi
Jinora
Korra
Kya
Mako
Tenzin
--END LIST--
(test-data/heros.txt)> mergeall
(test-data/heros.txt)> show
--BEG LIST--
Amon
Asami
Bolin
Bumi
Hiroshi
Jinora
Korra
Kuvira
Kya
Mako
Ming-Hua
P-li
Tenzin
Unalaq
Zaheer
--END LIST--
(test-data/heros.txt)> new fifth.txt
(fifth.txt)> close test-data/villains.txt
(fifth.txt)> close test-data/heros.txt
(fifth.txt)> edit second.txt
(second.txt)> add Korra
(second.txt)> add Hiroshi
(second.txt)> edit fourth.txt
(fourth.txt)> add Mako
(fourth.txt)> new sixth.txt
(sixth.txt)> showall
--List sixth.txt--

--List fifth.txt--

--List fourth.txt--
Asami
Mako

--List third.txt--
Bolin

--List second.txt--
Hiroshi
Korra
Mako

--List default.txt--
Asami
Bolin
Bumi
Jinora
Korra
Mako

(sixth.txt)> show
--BEG LIST--
--END LIST--
(sixth.txt)> mergeall
(sixth.txt)> show
--BEG LIST--
Asami
Bolin
Bumi
Hiroshi
Jinora
Korra
Mako
--END LIST--
(sixth.txt)> 
Lists multi-managed!
ENDOUT

((T++))
tnames[T]="stress"
read  -r -d '' input[$T] <<"ENDIN"
open test-data/common-1.txt
open test-data/common-2.txt
open test-data/common-3.txt
close default.txt
lists
open test-data/common-2.txt
lists
new test-data/first.tmp
add party
add or
add way
add guess
show
merge test-data/common-2.txt
show
showall
addall hospital
addall professor
addall relate
addall tv
showall
new test-data/second.tmp
lists
show
merge test-data/common-1.txt
show
merge test-data/common-3.txt
show
new test-data/third.tmp
mergeall
close test-data/common-3.txt
close test-data/common-1.txt
lists
close test-data/common-2.txt
lists
close test-data/common-1.txt
showall
saveall
new fourth.txt
close test-data/first.tmp
close test-data/second.tmp
close test-data/third.tmp
load test-data/first.tmp
show
undo
redo
show
clear
load test-data/second.tmp
mergein test-data/third.tmp
open test-data/first.tmp
showall
quit
ENDIN
#
read  -r -d '' output[$T] <<"ENDOUT"
(default.txt)> open test-data/common-1.txt
(test-data/common-1.txt)> open test-data/common-2.txt
(test-data/common-2.txt)> open test-data/common-3.txt
(test-data/common-3.txt)> close default.txt
(test-data/common-3.txt)> lists
3 docs
- test-data/common-3.txt
- test-data/common-2.txt
- test-data/common-1.txt
(test-data/common-3.txt)> open test-data/common-2.txt
ERROR: list 'test-data/common-2.txt' already exists
(test-data/common-3.txt)> lists
3 docs
- test-data/common-3.txt
- test-data/common-2.txt
- test-data/common-1.txt
(test-data/common-3.txt)> new test-data/first.tmp
(test-data/first.tmp)> add party
(test-data/first.tmp)> add or
(test-data/first.tmp)> add way
(test-data/first.tmp)> add guess
(test-data/first.tmp)> show
--BEG LIST--
guess
or
party
way
--END LIST--
(test-data/first.tmp)> merge test-data/common-2.txt
(test-data/first.tmp)> show
--BEG LIST--
act
administration
agree
and
any
debate
dream
early
environment
give
guess
hospital
leave
listen
network
or
out
pain
party
pm
project
pull
run
scientist
something
soon
tough
until
way
yourself
--END LIST--
(test-data/first.tmp)> showall
--List test-data/first.tmp--
act
administration
agree
and
any
debate
dream
early
environment
give
guess
hospital
leave
listen
network
or
out
pain
party
pm
project
pull
run
scientist
something
soon
tough
until
way
yourself

--List test-data/common-3.txt--
ability
another
around
check
design
effort
even
green
guess
my
political
program
relate
role
series
serious
service
single
stand
strategy
suggest
task
thus
tv
type

--List test-data/common-2.txt--
act
administration
agree
and
any
debate
dream
early
environment
give
guess
hospital
leave
listen
network
or
out
pain
party
pm
project
pull
run
scientist
something
soon
tough
until
way
yourself

--List test-data/common-1.txt--
after
bit
career
enough
far
floor
involve
morning
nation
nearly
professor
security
soon
sure
thing
try
violence
water
whom
worker

(test-data/first.tmp)> addall hospital
(test-data/first.tmp)> addall professor
(test-data/first.tmp)> addall relate
(test-data/first.tmp)> addall tv
(test-data/first.tmp)> showall
--List test-data/first.tmp--
act
administration
agree
and
any
debate
dream
early
environment
give
guess
hospital
leave
listen
network
or
out
pain
party
pm
professor
project
pull
relate
run
scientist
something
soon
tough
tv
until
way
yourself

--List test-data/common-3.txt--
ability
another
around
check
design
effort
even
green
guess
hospital
my
political
professor
program
relate
role
series
serious
service
single
stand
strategy
suggest
task
thus
tv
type

--List test-data/common-2.txt--
act
administration
agree
and
any
debate
dream
early
environment
give
guess
hospital
leave
listen
network
or
out
pain
party
pm
professor
project
pull
relate
run
scientist
something
soon
tough
tv
until
way
yourself

--List test-data/common-1.txt--
after
bit
career
enough
far
floor
hospital
involve
morning
nation
nearly
professor
relate
security
soon
sure
thing
try
tv
violence
water
whom
worker

(test-data/first.tmp)> new test-data/second.tmp
(test-data/second.tmp)> lists
5 docs
- test-data/second.tmp
- test-data/first.tmp
- test-data/common-3.txt
- test-data/common-2.txt
- test-data/common-1.txt
(test-data/second.tmp)> show
--BEG LIST--
--END LIST--
(test-data/second.tmp)> merge test-data/common-1.txt
(test-data/second.tmp)> show
--BEG LIST--
after
bit
career
enough
far
floor
hospital
involve
morning
nation
nearly
professor
relate
security
soon
sure
thing
try
tv
violence
water
whom
worker
--END LIST--
(test-data/second.tmp)> merge test-data/common-3.txt
(test-data/second.tmp)> show
--BEG LIST--
ability
after
another
around
bit
career
check
design
effort
enough
even
far
floor
green
guess
hospital
involve
morning
my
nation
nearly
political
professor
program
relate
role
security
series
serious
service
single
soon
stand
strategy
suggest
sure
task
thing
thus
try
tv
type
violence
water
whom
worker
--END LIST--
(test-data/second.tmp)> new test-data/third.tmp
(test-data/third.tmp)> mergeall
(test-data/third.tmp)> close test-data/common-3.txt
(test-data/third.tmp)> close test-data/common-1.txt
(test-data/third.tmp)> lists
4 docs
- test-data/third.tmp
- test-data/second.tmp
- test-data/first.tmp
- test-data/common-2.txt
(test-data/third.tmp)> close test-data/common-2.txt
(test-data/third.tmp)> lists
3 docs
- test-data/third.tmp
- test-data/second.tmp
- test-data/first.tmp
(test-data/third.tmp)> close test-data/common-1.txt
ERROR: list 'test-data/common-1.txt' does not exist
(test-data/third.tmp)> showall
--List test-data/third.tmp--
ability
act
administration
after
agree
and
another
any
around
bit
career
check
debate
design
dream
early
effort
enough
environment
even
far
floor
give
green
guess
hospital
involve
leave
listen
morning
my
nation
nearly
network
or
out
pain
party
pm
political
professor
program
project
pull
relate
role
run
scientist
security
series
serious
service
single
something
soon
stand
strategy
suggest
sure
task
thing
thus
tough
try
tv
type
until
violence
water
way
whom
worker
yourself

--List test-data/second.tmp--
ability
after
another
around
bit
career
check
design
effort
enough
even
far
floor
green
guess
hospital
involve
morning
my
nation
nearly
political
professor
program
relate
role
security
series
serious
service
single
soon
stand
strategy
suggest
sure
task
thing
thus
try
tv
type
violence
water
whom
worker

--List test-data/first.tmp--
act
administration
agree
and
any
debate
dream
early
environment
give
guess
hospital
leave
listen
network
or
out
pain
party
pm
professor
project
pull
relate
run
scientist
something
soon
tough
tv
until
way
yourself

(test-data/third.tmp)> saveall
(test-data/third.tmp)> new fourth.txt
(fourth.txt)> close test-data/first.tmp
(fourth.txt)> close test-data/second.tmp
(fourth.txt)> close test-data/third.tmp
(fourth.txt)> load test-data/first.tmp
(fourth.txt)> show
--BEG LIST--
act
administration
agree
and
any
debate
dream
early
environment
give
guess
hospital
leave
listen
network
or
out
pain
party
pm
professor
project
pull
relate
run
scientist
something
soon
tough
tv
until
way
yourself
--END LIST--
(fourth.txt)> undo
(fourth.txt)> redo
(fourth.txt)> show
--BEG LIST--
act
administration
agree
and
any
debate
dream
early
environment
give
guess
hospital
leave
listen
network
or
out
pain
party
pm
professor
project
pull
relate
run
scientist
something
soon
tough
tv
until
way
yourself
--END LIST--
(fourth.txt)> clear
(fourth.txt)> load test-data/second.tmp
(fourth.txt)> mergein test-data/third.tmp
(fourth.txt)> open test-data/first.tmp
(test-data/first.tmp)> showall
--List test-data/first.tmp--
act
administration
agree
and
any
debate
dream
early
environment
give
guess
hospital
leave
listen
network
or
out
pain
party
pm
professor
project
pull
relate
run
scientist
something
soon
tough
tv
until
way
yourself

--List fourth.txt--
ability
act
administration
after
agree
and
another
any
around
bit
career
check
debate
design
dream
early
effort
enough
environment
even
far
floor
give
green
guess
hospital
involve
leave
listen
morning
my
nation
nearly
network
or
out
pain
party
pm
political
professor
program
project
pull
relate
role
run
scientist
security
series
serious
service
single
something
soon
stand
strategy
suggest
sure
task
thing
thus
tough
try
tv
type
until
violence
water
way
whom
worker
yourself

(test-data/first.tmp)> quit

Lists multi-managed!
ENDOUT
