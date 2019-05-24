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
LM> help
LIST MANAGER
Maintains a sorted list without duplicates
No spaces allowed list data, use _ instead as in Lin_Beifong
Commands:
  help           : print this help message
  quit           : quit the program
  show           : print the current list to the screen
  clear          : set the list to empty, preserves undo history
  add <elem>     : add elem to the list
  remove <elem>  : remove elem from list
  save <file>    : save the current list to named file (not undoable)
  load <file>    : discard the current list, load sorted list in named file (undoable)
  mergein <file> : load the sorted list in the named file and merge with current list (undoable)
  undo           : undo the last operation restoring list to a previous state
  redo           : redo the last undone operation restoring list to a previous state
LM> quit

List managed!
ENDOUT

((T++))
tnames[T]="show-empty"
read  -r -d '' input[$T] <<"ENDIN"
show
show
quit
ENDIN
#
read  -r -d '' output[$T] <<"ENDOUT"
LM> show
--BEG LIST--
--END LIST--
LM> show
--BEG LIST--
--END LIST--
LM> quit

List managed!
ENDOUT

((T++))
tnames[T]="add-show"
read  -r -d '' input[$T] <<"ENDIN"
add Korra
show
add Mako
show
add Bolin
show
add Korra
show
ENDIN
#
read  -r -d '' output[$T] <<"ENDOUT"
LM> add Korra
LM> show
--BEG LIST--
Korra
--END LIST--
LM> add Mako
LM> show
--BEG LIST--
Korra
Mako
--END LIST--
LM> add Bolin
LM> show
--BEG LIST--
Bolin
Korra
Mako
--END LIST--
LM> add Korra
LM> show
--BEG LIST--
Bolin
Korra
Mako
--END LIST--
LM> 
List managed!
ENDOUT


((T++))
tnames[T]="add-remove"
read  -r -d '' input[$T] <<"ENDIN"
add Korra
add Mako
add Bolin
add Kuvira
add Tenzin
add Asami
show
remove Korra
remove Asami
remove Kuvira
show
add Meelo
add Pema
add Bumi
add Kya
show
ENDIN
#
read  -r -d '' output[$T] <<"ENDOUT"
LM> add Korra
LM> add Mako
LM> add Bolin
LM> add Kuvira
LM> add Tenzin
LM> add Asami
LM> show
--BEG LIST--
Asami
Bolin
Korra
Kuvira
Mako
Tenzin
--END LIST--
LM> remove Korra
LM> remove Asami
LM> remove Kuvira
LM> show
--BEG LIST--
Bolin
Mako
Tenzin
--END LIST--
LM> add Meelo
LM> add Pema
LM> add Bumi
LM> add Kya
LM> show
--BEG LIST--
Bolin
Bumi
Kya
Mako
Meelo
Pema
Tenzin
--END LIST--
LM> 
List managed!
ENDOUT


((T++))
tnames[T]="remove-not-there"
read  -r -d '' input[$T] <<"ENDIN"
add Korra
add Mako
add Bolin
add Kuvira
add Tenzin
add Asami
show
show
remove Meelo
remove Bumi
remove Zahir
show
ENDIN
#
read  -r -d '' output[$T] <<"ENDOUT"
LM> add Korra
LM> add Mako
LM> add Bolin
LM> add Kuvira
LM> add Tenzin
LM> add Asami
LM> show
--BEG LIST--
Asami
Bolin
Korra
Kuvira
Mako
Tenzin
--END LIST--
LM> show
--BEG LIST--
Asami
Bolin
Korra
Kuvira
Mako
Tenzin
--END LIST--
LM> remove Meelo
LM> remove Bumi
LM> remove Zahir
LM> show
--BEG LIST--
Asami
Bolin
Korra
Kuvira
Mako
Tenzin
--END LIST--
LM> 
List managed!
ENDOUT

((T++))
tnames[T]="save-load1"
read  -r -d '' input[$T] <<"ENDIN"
add Korra
add Mako
add Bolin
add Kuvira
add Tenzin
add Asami
save test-data/a.tmp
clear
show
load test-data/a.tmp
show
ENDIN
#
read  -r -d '' output[$T] <<"ENDOUT"
LM> add Korra
LM> add Mako
LM> add Bolin
LM> add Kuvira
LM> add Tenzin
LM> add Asami
LM> save test-data/a.tmp
LM> clear
LM> show
--BEG LIST--
--END LIST--
LM> load test-data/a.tmp
LM> show
--BEG LIST--
Asami
Bolin
Korra
Kuvira
Mako
Tenzin
--END LIST--
LM> 
List managed!
ENDOUT

((T++))
tnames[T]="load-replaces"
read  -r -d '' input[$T] <<"ENDIN"
add Korra
add Mako
add Bolin
add Kuvira
add Tenzin
add Meelo
add Asami
save test-data/a.tmp
clear
add Tenzin
add Amon
add Pema
add Bumi
add Meelo
show
load test-data/a.tmp
show
ENDIN
#
read  -r -d '' output[$T] <<"ENDOUT"
LM> add Korra
LM> add Mako
LM> add Bolin
LM> add Kuvira
LM> add Tenzin
LM> add Meelo
LM> add Asami
LM> save test-data/a.tmp
LM> clear
LM> add Tenzin
LM> add Amon
LM> add Pema
LM> add Bumi
LM> add Meelo
LM> show
--BEG LIST--
Amon
Bumi
Meelo
Pema
Tenzin
--END LIST--
LM> load test-data/a.tmp
LM> show
--BEG LIST--
Asami
Bolin
Korra
Kuvira
Mako
Meelo
Tenzin
--END LIST--
LM> 
List managed!
ENDOUT

((T++))
tnames[T]="load-replaces"
read  -r -d '' input[$T] <<"ENDIN"
add Korra
add Mako
add Bolin
add Kuvira
add Tenzin
add Meelo
add Asami
save test-data/a.tmp
clear
add Tenzin
add Amon
add Pema
add Bumi
add Meelo
show
load test-data/a.tmp
show
ENDIN
#
read  -r -d '' output[$T] <<"ENDOUT"
LM> add Korra
LM> add Mako
LM> add Bolin
LM> add Kuvira
LM> add Tenzin
LM> add Meelo
LM> add Asami
LM> save test-data/a.tmp
LM> clear
LM> add Tenzin
LM> add Amon
LM> add Pema
LM> add Bumi
LM> add Meelo
LM> show
--BEG LIST--
Amon
Bumi
Meelo
Pema
Tenzin
--END LIST--
LM> load test-data/a.tmp
LM> show
--BEG LIST--
Asami
Bolin
Korra
Kuvira
Mako
Meelo
Tenzin
--END LIST--
LM> 
List managed!
ENDOUT

((T++))
tnames[T]="save-load-mult"
read  -r -d '' input[$T] <<"ENDIN"
add Korra
add Mako
add Bolin
add Kuvira
add Tenzin
add Meelo
add Asami
save test-data/a.tmp
clear
add Tenzin
add Amon
add Pema
add Bumi
add Meelo
save test-data/b.tmp
load test-data/a.tmp
show
load test-data/b.tmp
show
remove Amon
add Korra
show
load test-data/a.tmp
show
load test-data/b.tmp
show
ENDIN
#
read  -r -d '' output[$T] <<"ENDOUT"
LM> add Korra
LM> add Mako
LM> add Bolin
LM> add Kuvira
LM> add Tenzin
LM> add Meelo
LM> add Asami
LM> save test-data/a.tmp
LM> clear
LM> add Tenzin
LM> add Amon
LM> add Pema
LM> add Bumi
LM> add Meelo
LM> save test-data/b.tmp
LM> load test-data/a.tmp
LM> show
--BEG LIST--
Asami
Bolin
Korra
Kuvira
Mako
Meelo
Tenzin
--END LIST--
LM> load test-data/b.tmp
LM> show
--BEG LIST--
Amon
Bumi
Meelo
Pema
Tenzin
--END LIST--
LM> remove Amon
LM> add Korra
LM> show
--BEG LIST--
Bumi
Korra
Meelo
Pema
Tenzin
--END LIST--
LM> load test-data/a.tmp
LM> show
--BEG LIST--
Asami
Bolin
Korra
Kuvira
Mako
Meelo
Tenzin
--END LIST--
LM> load test-data/b.tmp
LM> show
--BEG LIST--
Amon
Bumi
Meelo
Pema
Tenzin
--END LIST--
LM> 
List managed!
ENDOUT

((T++))
tnames[T]="mergein1"
read  -r -d '' input[$T] <<"ENDIN"
add Korra
add Mako
add Asami
add Bolin
save test-data/a.tmp
clear
add Tenzin
add Pema
add Bumi
add Meelo
show
mergein test-data/a.tmp
show
ENDIN
#
read  -r -d '' output[$T] <<"ENDOUT"
LM> add Korra
LM> add Mako
LM> add Asami
LM> add Bolin
LM> save test-data/a.tmp
LM> clear
LM> add Tenzin
LM> add Pema
LM> add Bumi
LM> add Meelo
LM> show
--BEG LIST--
Bumi
Meelo
Pema
Tenzin
--END LIST--
LM> mergein test-data/a.tmp
LM> show
--BEG LIST--
Asami
Bolin
Bumi
Korra
Mako
Meelo
Pema
Tenzin
--END LIST--
LM> 
List managed!
ENDOUT

((T++))
tnames[T]="mergein2"
read  -r -d '' input[$T] <<"ENDIN"
load test-data/heros.txt
show
mergein test-data/villains.txt
show
clear
add Korra
add Bolin
mergein test-data/heros.txt
show
add Kuvira
add Tenzin
add P-li
mergein test-data/villains.txt
show
quit
ENDIN
#
read  -r -d '' output[$T] <<"ENDOUT"
LM> load test-data/heros.txt
LM> show
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
LM> mergein test-data/villains.txt
LM> show
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
Tonraq
Zaheer
--END LIST--
LM> clear
LM> add Korra
LM> add Bolin
LM> mergein test-data/heros.txt
LM> show
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
LM> add Kuvira
LM> add Tenzin
LM> add P-li
LM> mergein test-data/villains.txt
LM> show
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
Tonraq
Zaheer
--END LIST--
LM> quit

List managed!
ENDOUT

((T++))
tnames[T]="undo-redo-warn"
read  -r -d '' input[$T] <<"ENDIN"
undo
redo
add Korra
redo
undo
undo
ENDIN
#
read  -r -d '' output[$T] <<"ENDOUT"
LM> undo
WARNING: undo list empty, no changes made
LM> redo
WARNING: redo list empty, no changes made
LM> add Korra
LM> redo
WARNING: redo list empty, no changes made
LM> undo
LM> undo
WARNING: undo list empty, no changes made
LM> 
List managed!
ENDOUT

((T++))
tnames[T]="undo-redo1"
read  -r -d '' input[$T] <<"ENDIN"
add Korra
add Mako
add Asami
add Bolin
undo
show
undo
show
redo 
show
redo 
show
remove Asami
remove Mako
show
undo
show
undo
show
redo
show
ENDIN
#
read  -r -d '' output[$T] <<"ENDOUT"
LM> add Korra
LM> add Mako
LM> add Asami
LM> add Bolin
LM> undo
LM> show
--BEG LIST--
Asami
Korra
Mako
--END LIST--
LM> undo
LM> show
--BEG LIST--
Korra
Mako
--END LIST--
LM> redo 
LM> show
--BEG LIST--
Asami
Korra
Mako
--END LIST--
LM> redo 
LM> show
--BEG LIST--
Asami
Bolin
Korra
Mako
--END LIST--
LM> remove Asami
LM> remove Mako
LM> show
--BEG LIST--
Bolin
Korra
--END LIST--
LM> undo
LM> show
--BEG LIST--
Bolin
Korra
Mako
--END LIST--
LM> undo
LM> show
--BEG LIST--
Asami
Bolin
Korra
Mako
--END LIST--
LM> redo
LM> show
--BEG LIST--
Bolin
Korra
Mako
--END LIST--
LM> 
List managed!
ENDOUT

((T++))
tnames[T]="undo-redo2"
read  -r -d '' input[$T] <<"ENDIN"
add Korra
add Mako
add Asami
add Bolin
show
undo
undo
show
redo
show
remove Mako
show
add Tenzin
add Bumi
add Kya
add Meelo
show
undo
undo
undo
redo
redo
show
clear
show
undo
show
mergein test-data/heros.txt
show
undo
show
redo
show
load test-data/villains.txt
add Eska
show
undo
undo
show
redo
show
ENDIN
#
read  -r -d '' output[$T] <<"ENDOUT"
LM> add Korra
LM> add Mako
LM> add Asami
LM> add Bolin
LM> show
--BEG LIST--
Asami
Bolin
Korra
Mako
--END LIST--
LM> undo
LM> undo
LM> show
--BEG LIST--
Korra
Mako
--END LIST--
LM> redo
LM> show
--BEG LIST--
Asami
Korra
Mako
--END LIST--
LM> remove Mako
LM> show
--BEG LIST--
Asami
Korra
--END LIST--
LM> add Tenzin
LM> add Bumi
LM> add Kya
LM> add Meelo
LM> show
--BEG LIST--
Asami
Bumi
Korra
Kya
Meelo
Tenzin
--END LIST--
LM> undo
LM> undo
LM> undo
LM> redo
LM> redo
LM> show
--BEG LIST--
Asami
Bumi
Korra
Kya
Tenzin
--END LIST--
LM> clear
LM> show
--BEG LIST--
--END LIST--
LM> undo
LM> show
--BEG LIST--
Asami
Bumi
Korra
Kya
Tenzin
--END LIST--
LM> mergein test-data/heros.txt
LM> show
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
LM> undo
LM> show
--BEG LIST--
Asami
Bumi
Korra
Kya
Tenzin
--END LIST--
LM> redo
LM> show
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
LM> load test-data/villains.txt
LM> add Eska
LM> show
--BEG LIST--
Amon
Eska
Hiroshi
Kuvira
Ming-Hua
P-li
Tonraq
Zaheer
--END LIST--
LM> undo
LM> undo
LM> show
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
LM> redo
LM> show
--BEG LIST--
Amon
Hiroshi
Kuvira
Ming-Hua
P-li
Tonraq
Zaheer
--END LIST--
LM> 
List managed!
ENDOUT


((T++))
tnames[T]="stress"
read  -r -d '' input[$T] <<"ENDIN"
load test-data/villains.txt
mergein test-data/heros.txt
add Korra
add Bolin
add Mako
undo
undo
show
undo
undo
show
redo
remove Amon
remove Zaheer
remove Void
remove Null
show
save test-data/x.tmp
remove Tenzin
remove Pema
remove Meelo
remove Mako
show
mergein test-data/x.tmp
show
undo
undo
show
redo
clear
load test-data/villains.txt
add Korra
add Bumi
remove Kuvira
load test-data/heros.txt
undo
mergein test-data/heros.txt
show
undo
show
redo
redo
remove Kuvira
add Eska
save test-data/y.tmp
load test-data/heros.txt
show
mergein test-data/y.tmp
show
ENDIN
#
read  -r -d '' output[$T] <<"ENDOUT"
LM> load test-data/villains.txt
LM> mergein test-data/heros.txt
LM> add Korra
LM> add Bolin
LM> add Mako
LM> undo
LM> undo
LM> show
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
Tonraq
Zaheer
--END LIST--
LM> undo
LM> undo
LM> show
--BEG LIST--
Amon
Hiroshi
Kuvira
Ming-Hua
P-li
Tonraq
Zaheer
--END LIST--
LM> redo
LM> remove Amon
LM> remove Zaheer
LM> remove Void
LM> remove Null
LM> show
--BEG LIST--
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
Tonraq
--END LIST--
LM> save test-data/x.tmp
LM> remove Tenzin
LM> remove Pema
LM> remove Meelo
LM> remove Mako
LM> show
--BEG LIST--
Asami
Bolin
Bumi
Hiroshi
Jinora
Korra
Kuvira
Kya
Ming-Hua
P-li
Tonraq
--END LIST--
LM> mergein test-data/x.tmp
LM> show
--BEG LIST--
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
Tonraq
--END LIST--
LM> undo
LM> undo
LM> show
--BEG LIST--
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
Tonraq
--END LIST--
LM> redo
LM> clear
LM> load test-data/villains.txt
LM> add Korra
LM> add Bumi
LM> remove Kuvira
LM> load test-data/heros.txt
LM> undo
LM> mergein test-data/heros.txt
LM> show
--BEG LIST--
Amon
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
Tonraq
Zaheer
--END LIST--
LM> undo
LM> show
--BEG LIST--
Amon
Bumi
Hiroshi
Korra
Ming-Hua
P-li
Tonraq
Zaheer
--END LIST--
LM> redo
LM> redo
WARNING: redo list empty, no changes made
LM> remove Kuvira
LM> add Eska
LM> save test-data/y.tmp
LM> load test-data/heros.txt
LM> show
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
LM> mergein test-data/y.tmp
LM> show
--BEG LIST--
Amon
Asami
Bolin
Bumi
Eska
Hiroshi
Jinora
Korra
Kya
Mako
Ming-Hua
P-li
Tenzin
Tonraq
Zaheer
--END LIST--
LM> 
List managed!
ENDOUT

