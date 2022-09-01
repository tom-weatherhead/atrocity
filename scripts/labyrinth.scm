; Note: This is a pseudo-Scheme file.
; Labyrinth in Scheme - November 21, 2013

; BEGIN Define commonly-used lambda expressions here.
; Of particular importance are combine, compose, and curry.

(set id (lambda (x) x))

(set combine (lambda (f sum zero) ; Version 2, using letrec: see page 126
	(letrec
		((loop (lambda (l) (if (null? l) zero (sum (f (car l)) (loop (cdr l)))))))
		loop
	)
))

(set compose (lambda (f g) (lambda (x) (g (f x)))))

(set curry (lambda (f) (lambda (x) (lambda (y) (f x y)))))

(set compose2args (lambda (f g) (lambda (x y) (g (f x y)))))
(set reverse2args (lambda (f) (lambda (x y) (f y x))))

; (set > (reverse2args <)) ; Comment out if Scheme implements > as a primop
(set not (lambda (x) (if x '() 'T)))
(set and (lambda (x y) (if x y x)))
(set or (lambda (x y) (if x x y)))
(set mod (lambda (m n) (- m (* n (/ m n)))))
(set gcd (lambda (m n) (if (= n 0) m (gcd n (mod m n)))))

; (set atom? (lambda (x) (or (null? x) (or (number? x) (or (symbol? x) (string? x)))))) ; What about primop? and closure? ?
(set atom? (compose list? not)) ; Version 2

; (set equal (lambda (l1 l2) (if (atom? l1) (= l1 l2) (if (atom? l2) '() (if (equal (car l1) (car l2)) (equal (cdr l1) (cdr l2)) '()))))) ; Version 1
(set equal (lambda (l1 l2) (cond ((atom? l1) (= l1 l2)) ((atom? l2) '()) ((equal (car l1) (car l2)) (equal (cdr l1) (cdr l2))) ('T '()) ))) ; Version 2

; (set >= (compose2args < not)) ; Comment out if Scheme implements >= as a primop
; (set <= (compose2args > not)) ; Comment out if Scheme implements <= as a primop
(set <> (compose2args = not))
(set any (lambda (l) (if (null? l) '() (if (car l) 'T (any (cdr l))))))
(set all (lambda (l) (if (null? l) 'T (if (not (car l)) '() (all (cdr l))))))

; (set mapcar (lambda (f l) (if (null? l) '() (cons (f (car l)) (mapcar f (cdr l)))))) ; Original definition.
; (set mapc (curry mapcar)) ; Original definition.  From page 101.
(set mapc (lambda (f) (combine f cons '()))) ; Second definition.
(set mapcar (lambda (f l) ((mapc f) l))) ; Second definition.

(set any2 (combine id or '()))
(set all2 (combine id and 'T))

; (set +1 (lambda (n) (+ n 1))) ; Version 1
(set +1 ((curry +) 1)) ; Version 2

; (set append (lambda (l1 l2) (if (null? l1) l2 (cons (car l1) (append (cdr l1) l2))))) ; Version 1
(set append (lambda (l1 l2) ((combine id cons l2) l1))) ; Version 2

(set reverse (lambda (l) (letrec ((rev-aux (lambda (l1 l2) (if (null? l1) l2 (rev-aux (cdr l1) (cons (car l1) l2)))))) (rev-aux l '()))))
(set skip (lambda (n l) (if (or (null? l) (= n 0)) l (skip (- n 1) (cdr l)))))
(set take (lambda (n l) (if (or (null? l) (= n 0)) '() (cons (car l) (take (- n 1) (cdr l))))))
(set abs (lambda (n) (if (< n 0) (- 0 n) n)))

; (set cadr (lambda (l) (car (cdr l)))) ; Version 1
(set cadr (compose cdr car)) ; Version 2

(set length (lambda (l) (if (null? l) 0 (+1 (length (cdr l)))))) ; Adapted from page 29.

; (set find (lambda (pred lis) ; From page 104
; (if (null? lis) '()
; (if (pred (car lis)) 'T (find pred (cdr lis)))))) ; Version 1

; Version 2
(set find (lambda (pred lis) (cond ((null? lis) '()) ((pred (car lis)) 'T) ('T (find pred (cdr lis))) ) ))

(set nth (lambda (n l) (if (= n 0) (car l) (nth (- n 1) (cdr l))))) ; Adapted from page 43.

; END Define commonly-used lambda expressions here.

; loadPreset assoc
; Association list functions (adapted from page 32)
(set caar (compose car car))
(set cadar (compose car cadr))
(set assoc (lambda (x alist)
	(cond
		((null? alist) '())
		((= x (caar alist)) (cadar alist))
		('T (assoc x (cdr alist)))
	)
))
(set mkassoc (lambda (x y alist)
	(cond
		((null? alist) (list (list x y)))
		((= x (caar alist)) (cons (list x y) (cdr alist)))
		('T (cons (car alist) (mkassoc x y (cdr alist))))
	)
))

(set assoc-contains-key (lambda (x alist) (find (compose car ((curry =) x)) alist)))

; Adapted from page 55
(set rplac-assoc (lambda (x y alist)
	(cond
		((null? alist) '())
		((= x (caar alist)) (rplacd (car alist) (list y)))
		((null? (cdr alist)) (rplacd alist (list (list x y))))
		('T (rplac-assoc x y (cdr alist)))
	)
))

; loadPreset filter

(set filter (lambda (pred l) ; Returns only the elements of l for which pred is true.
	(cond
		((null? l) '())
		((pred (car l)) (cons (car l) (filter pred (cdr l))))
		('T (filter pred (cdr l)))
	)
))
(set remove (lambda (x l) ; Returns a copy of l that has had all occurrences of x removed.
	(filter (compose ((curry =) x) not) l)
))

; loadPreset queue

; Queue functions (adapted from page 37)
(set empty-queue '())
(set front car)
(set rm-front cdr)

; (set enqueue (lambda (t q) (if (null? q) (list t) (cons (car q) (enqueue t (cdr q)))))) ; Version 1
(set enqueue (lambda (t q) (append q (list t)))) ; Version 2; 2013/11/30

(set empty? null?)

; loadPreset set

; Scheme set functions; from pages 104-105
(set nullset '())
(set member? (lambda (x s) (find ((curry equal) x) s)))
(set addelt (lambda (x s) (if (member? x s) s (cons x s))))
(set union (lambda (s1 s2) ((combine id addelt s1) s2)))

; loadPreset stack

(set empty-stack '())
(set push cons)
(set peek car)
(set pop cdr)
(set empty-stack? null?)

; loadPreset sublist

(set sublist (lambda (l start len) (cond ((or (<= len 0) (null? l)) '()) ((> start 0) (sublist (cdr l) (- start 1) len)) ('T (cons (car l) (sublist (cdr l) 0 (- len 1)))))))

(set removesublist (lambda (l start len) (cond ((or (<= len 0) (null? l)) l) ((> start 0) (cons (car l) (removesublist (cdr l) (- start 1) len))) ('T (removesublist (cdr l) 0 (- len 1))))))

; **** class RoomInfo ****

; A room is a 2-list: (level-number room-number)
(set levelNumber car)
(set roomNumber cadr)

; (to 0 5) yields (0 1 2 3 4 5)
(set to (lambda (start end) (if (> start end) '() (cons start (to (+1 start) end)))))

(set generatePossibleNeighboursOnLevel (lambda (room newLevel numberOfRoomsPerLevel)
	(let ((rn (roomNumber room)) (centreRoomNumber (- numberOfRoomsPerLevel 1)))
		(if (= rn centreRoomNumber)
			(mapcar (lambda (i) (list newLevel i)) (to 0 (- numberOfRoomsPerLevel 2)))
			(list
				(list newLevel (mod (+1 rn) centreRoomNumber))
				(list newLevel (mod (- (+ rn centreRoomNumber) 1) centreRoomNumber))
				(list newLevel centreRoomNumber)
			)
		)
	)
))

(set generatePossibleNeighbours (lambda (room numberOfLevels numberOfRoomsPerLevel)
	(let ((levNum (levelNumber room)))
		(append
			(if (> levNum 0) (generatePossibleNeighboursOnLevel room (- levNum 1) numberOfRoomsPerLevel) '())
			(if (< levNum (- numberOfLevels 1)) (generatePossibleNeighboursOnLevel room (+1 levNum) numberOfRoomsPerLevel) '())
		)
	)
))

; **** class LabyrinthGenerator ****

(set constructor (lambda (l r) (begin
	(set numberOfLevels l)
	(set numberOfRoomsPerLevel r)
	(set numberOfExtraConnections 0)
	(set numberOfExtraConnectionsAdded 0)
	(set extraConnections '())
	(set rooms '())
	(set roomLabels '())
	(set connections '())
	(set openList '()) ; Is this a queue, a set, or a stack?
	(set numberOfDifferentLabels 0)
	(set roomGoal '())
	(set booksInRooms '())
	(set numberOfAttemptsToRefactor 0)
	(set maximumNumberOfAttemptsToRefactor 100)
	(set currentRoom '())
	(set visitedRooms '()) ; This is a set.
	(set JorgesRoom '())
	(set JorgesPath '())
)))

(set roomListContainsRoom (lambda (roomList room) ; For an alternative, see "member?" in the "set" preset.
	(find ((curry =) room) roomList)
))

(set areConnected (lambda (room1 room2)
	(member? room2 (assoc room1 connections))
))

(set findConflictingConnections (lambda (room1 room2)
	(let* (
		(l1 (levelNumber room1))
		(r1 (roomNumber room1))
		(l2 (levelNumber room2))
		(r2 (roomNumber room2))
		(room3 (list l1 r2))
		(room4 (list l2 r1))
		(l5 (- (* 2 l1) l2))
		(room5 (list l5 r2))
		(l6 (- (* 2 l2) l1))
		(room6 (list l6 r1))
		)
		(any (list
			(areConnected room3 room4)
			(areConnected room1 room5)
			(areConnected room2 room6)
		))
	)
))

(set labelIsUsed (lambda (label)
	(find
		(lambda (key-value-pair) (= (cadr key-value-pair) label))
		roomLabels
	)
))

(set findUnusedLabel (lambda ()
	(let ((result 0))
		(begin
			(while (labelIsUsed result)
				(set result (+1 result))
			)
			result
		)
	)
))

(set propagateNewLabel (lambda (room newLabel addRoomsToOpenList)
	(let (
		(openListStackLocal (list room))
		(closedListSet '())
		(roomFromOpenList '())
		)
		(while (not (empty? openListStackLocal)) (begin
			(set roomFromOpenList (peek openListStackLocal))
			(set openListStackLocal (pop openListStackLocal))
			(rplac-assoc roomFromOpenList newLabel roomLabels)
			(set closedListSet (addelt roomFromOpenList closedListSet))

			(if addRoomsToOpenList
				(set openList (addelt roomFromOpenList openList))
				'()
			)

			; foreach
			(mapcar
				(lambda (room2)
					(if (not (or (member? room2 openListStackLocal) (member? room2 closedListSet)))
						(set openListStackLocal (push room2 openListStackLocal))
						'()
					)
				)
				(assoc roomFromOpenList connections)
			)
		))
	)
))

(set findPossibleNeighboursWithDifferentLabels (lambda () ; Returns a list (room1 room2) or throws an exception.
	(let
		(
			(openListLocal rooms)
			(r 0) ; A random number
			(room1 '())
			(room2 '())
			(possibleNeighbours '())
		)
		(call/cc (lambda (exit) (begin

			(while (not (empty? openListLocal)) (begin
				(set r (random (length openListLocal)))
				(set room1 (nth r openListLocal))
				(set openListLocal (removesublist openListLocal r 1))
				(set possibleNeighbours (generatePossibleNeighbours room1 numberOfLevels numberOfRoomsPerLevel))

				(while (not (empty? possibleNeighbours)) (begin
					(set r (random (length possibleNeighbours)))
					(set room2 (nth r possibleNeighbours))
					(set possibleNeighbours (removesublist possibleNeighbours r 1))

					(if (<> (assoc room1 roomLabels) (assoc room2 roomLabels))
						(exit (list room1 room2))
						'()
					)
				))
			))
			(throw "Unable to find possible neighbours with different labels.")
		)))
	)
))

(set removeOneConnection (lambda (room1 room2)
	(rplac-assoc
		room1
		(remove room2 (assoc room1 connections))
		connections
	)
))

(set removeBothConnection (lambda (room1 room2)
	(begin
		(removeOneConnection room1 room2)
		(removeOneConnection room2 room1)
	)
))

(set numberOfUniqueRoomLabels (lambda ()
	(letrec ((loop (lambda (alist resultSet) (if (null? alist) (length resultSet) (loop (cdr alist) (addelt (cadar alist) resultSet))))))
		(loop roomLabels nullset)
	)
))

(set refactor (lambda ()
	(let* (
		(roomsOfInterest (findPossibleNeighboursWithDifferentLabels))
		(room1 (car roomsOfInterest))
		(room2 (cadr roomsOfInterest))
		(l1 (levelNumber room1))
		(r1 (roomNumber room1))
		(l2 (levelNumber room2))
		(r2 (roomNumber room2))
		(room3 (list l1 r2))
		(room4 (list l2 r1))
		(l5 (- (* 2 l1) l2))
		(room5 (list l5 r2))
		(l6 (- (* 2 l2) l1))
		(room6 (list l6 r1))
		)
		(begin
			(if (areConnected room3 room4)
				(begin
					(print "Found a Type 1 conflict.")
					(removeBothConnection room3 room4)
					(propagateNewLabel room3 (findUnusedLabel) 'T)
					(propagateNewLabel room4 (findUnusedLabel) 'T)
				)
				'()
			)
			(if (areConnected room1 room5)
				(begin
					(print "Found a Type 2 conflict.")
					(removeBothConnection room1 room5)
					(propagateNewLabel room5 (findUnusedLabel) 'T)
				)
				'()
			)
			(if (areConnected room2 room6)
				(begin
					(print "Found a Type 3 conflict.")
					(removeBothConnection room2 room6)
					(propagateNewLabel room6 (findUnusedLabel) 'T)
				)
				'()
			)

			; Connect room1 and room2.
			(propagateNewLabel room2 (assoc room1 roomLabels) '())
			(rplac-assoc room1 (cons room2 (assoc room1 connections)) connections)
			(rplac-assoc room2 (cons room1 (assoc room2 connections)) connections)

			(set numberOfDifferentLabels (numberOfUniqueRoomLabels))
		)
	)
))

(set finalValidityCheck (lambda ()
	(begin
		(propagateNewLabel '(0 0) (findUnusedLabel) '())

		(if (> (numberOfUniqueRoomLabels) 1)
			(throw "The labyrinth is in multiple blobs.")
			'()
		)

		(print "The labyrinth is a single blob.")
	)
))

(set generateRoomList (lambda (numberOfLevels numberOfRoomsPerLevel)
	((combine
		(lambda (l) (mapcar (lambda (r) (list l r)) (to 0 (- numberOfRoomsPerLevel 1))))
	append '()) (to 0 (- numberOfLevels 1)))
))

(set generateRoomLabelsList (lambda (rl n)
	(if (null? rl)
		'()
		(cons (list (car rl) n) (generateRoomLabelsList (cdr rl) (+1 n)))
	)
))

(set report (lambda () (begin
	(mapcar (lambda (room1)
		(mapcar (lambda (room2)
			(print (listtostring (list "Room " room1 " is connected to Room " room2 ".")))
		) (assoc room1 connections))
	) rooms)

	(cond
		((= numberOfAttemptsToRefactor 1) (print "The labyrinth was refactored 1 time."))
		((> numberOfAttemptsToRefactor 0) (print (listtostring (list "The labyrinth was refactored " numberOfAttemptsToRefactor " times."))))
		('T '())
	)

    (finalValidityCheck)
)))

(set findShortestPathBetweenRooms (lambda (room roomGoalLocal) ; roomGoalLocal may be null.
	(let (
		(openListLocal (list room)) ; This is a queue.
		(paths (list (list room (list room)))) ; This is a dictionary/association list; the keys are rooms, and the values are lists of rooms.
		(pathToRoom '())
		)
		(if (= room roomGoalLocal)
			(assoc room paths)
			(call/cc (lambda (exit) (begin
				(while (not (null? openListLocal)) (begin
					(set room (front openListLocal))
					(set openListLocal (rm-front openListLocal))
					(set pathToRoom (assoc room paths))
					(mapcar (lambda (room2)
						(if (not (assoc-contains-key room2 paths))
							(begin
								(set openListLocal (enqueue room2 openListLocal))
								(set paths (cons (list room2 (append pathToRoom (list room2))) paths))
								(if (= room2 roomGoalLocal)
									(exit (assoc room2 paths))
									'()
								)
							)
							'()
						)
					) (assoc room connections))
				))
				(assoc room paths) ; Or just pathToRoom ?
			)))
		)
	)
))

(set findLongestPathFromRoom (lambda (room) (findShortestPathBetweenRooms room '())))

(set printLongestPath (lambda ()
	(let* (
		(path1 (findLongestPathFromRoom (list (- numberOfLevels 1) (- numberOfRoomsPerLevel 1))))
		(longestPath (findLongestPathFromRoom (car (reverse path1))))
		)
		(begin
			; (print (cons "Longest path: " longestPath))
			(print (listtostring (list "The longest path contains " (length longestPath) " rooms.")))
			(set roomGoal (car (reverse longestPath)))
			; (print (listtostring (list "Aristotle's Second Book of the Poetics is in Room " roomGoal ".")))
			(print (listtostring (list "The path from Room (0 0) to the goal contains " (length (findShortestPathBetweenRooms '(0 0) roomGoal)) " rooms.")))
		)
	)
))

(set placeBooksInRooms (lambda () ; A room can contain multiple books.
	(let (
		(books (list
			"The First Book of the Poetics of Aristotle"
			"The Iliad by Homer"
			"The Odyssey by Homer"
			"The Republic by Plato"
			"Categories by Aristotle"
			"Physics by Aristotle"
			"Nicomachean Ethics by Aristotle"
			"The Aeneid by Virgil"
			"The Old Testament in Hebrew"
			"The New Testament in Greek"
			"Strong's Hebrew Dictionary"
			"Strong's Greek Dictionary"
		))
		(r 0)
		(numRooms (length rooms))
		(room '())
		)
		(begin
			(set booksInRooms (list (list roomGoal (list "The Second Book of the Poetics of Aristotle"))))
			(mapcar (lambda (book) (begin
				(set r (random numRooms))
				(set room (nth r rooms))
				(if (assoc-contains-key room booksInRooms)
					(rplac-assoc room (cons book (assoc room booksInRooms)) booksInRooms)
					(set booksInRooms (cons (list room (list book)) booksInRooms))
				)
			)) books)
		)
	)
))

(set reportProximityToJorge (lambda ()
	(let* (
		(path (findShortestPathBetweenRooms currentRoom JorgesRoom))
		(distance (- (length path) 1))
		)
		(cond
			((= distance 0) (begin
				(print "* You and the Venerable Jorge are in the same room! *")
				(print "'Good evening, Venerable Jorge.'")
			))
			((<= distance 2) (print "The Venerable Jorge is very near."))
			((<= distance 4) (print "The Venerable Jorge is near."))
			('T '())
		)
	)
))

(set constructJorgesPath (lambda () ; This returns Jorge's new path.
	(let ((JorgesGoal '()))
		(call/cc (lambda (exit)
			(while 'T (begin
				(set JorgesGoal (nth (random (length rooms)) rooms))
				(if (<> JorgesRoom JorgesGoal)
					(exit (cdr (findShortestPathBetweenRooms JorgesRoom JorgesGoal))) ; Use cdr because the first room is Jorge's current room.
					'()
				)
			))
		))
	)
))

(set printAdjacentRooms (lambda (n adjacentRooms)
	(if (null? adjacentRooms)
		'()
		(begin
			(print (listtostring (list n ": " (car adjacentRooms) (if (roomListContainsRoom visitedRooms (car adjacentRooms)) " Visited" ""))))
			(printAdjacentRooms (+1 n) (cdr adjacentRooms))
		)
	)
))

(set moveToRoom (lambda (room) (begin
	; Move the player to the given room.
	(set currentRoom room)
	(set visitedRooms (addelt room visitedRooms))
	(print (listtostring (list "You are in Room " room ".")))
	(reportProximityToJorge)
	(if (assoc-contains-key room booksInRooms)
		(mapcar (lambda (book)
			(print (listtostring (list "This room contains the book '" book "'.")))
		) (assoc room booksInRooms))
		'()
	)
	(if (= room roomGoal)
		(print "**** Congratulations!  You have reached the goal! ****")
		'()
	)

	; Move Jorge.
	(print "Jorge moves.")
	(if (null? JorgesPath)
		(set JorgesPath (constructJorgesPath))
		'()
	)
	(set JorgesRoom (car JorgesPath))
	(set JorgesPath (cdr JorgesPath))
	; (print (listtostring (list "The Venerable Jorge is in Room " JorgesRoom ".")))
	(reportProximityToJorge)

	(print "AdjacentRooms")
	(print (assoc room connections))

	(printAdjacentRooms 0 (assoc room connections))
)))

(set generate (lambda (numberOfLevels numberOfRoomsPerLevel)
	(let (
		(room1Index 0)
		(room1 '())
		(possibleNeighbours '())
		(room2 '())
		(room2Index 0)
		(room2Temp '())
		(label1 0)
		(label2 0)
		(minLabel 0)
		(maxLabel 0)
		)
		(begin
			(constructor numberOfLevels numberOfRoomsPerLevel)
			(set rooms (generateRoomList numberOfLevels numberOfRoomsPerLevel))
			(set connections (mapcar (lambda (r) (list r '())) rooms))
			(set numberOfDifferentLabels (length rooms))
			(set openList rooms)
			(set roomLabels (generateRoomLabelsList rooms 0))

			(while (> numberOfDifferentLabels 1) (call/cc (lambda (continue) (begin

				(if (null? openList)
					(begin

						(if (>= numberOfAttemptsToRefactor maximumNumberOfAttemptsToRefactor)
							(throw "Attempted to refactor too many times.")
							'()
						)

						(set numberOfAttemptsToRefactor (+1 numberOfAttemptsToRefactor))
						(refactor)
					)
					'()
				)

				(set room1Index (random (length openList)))
				(set room1 (nth room1Index openList))
				(set possibleNeighbours (generatePossibleNeighbours room1 numberOfLevels numberOfRoomsPerLevel))
				(set room2 (call/cc (lambda (exit) (begin
					(while (not (null? possibleNeighbours)) (begin
						(set room2Index (random (length possibleNeighbours)))
						(set room2Temp (nth room2Index possibleNeighbours))

						(if (and
							(<> (assoc room1 roomLabels) (assoc room2Temp roomLabels))
							(not (findConflictingConnections room1 room2Temp))
							)
							(exit room2Temp)
							'()
						)

						(set possibleNeighbours (removesublist possibleNeighbours room2Index 1))
					))
					(set openList (removesublist openList room1Index 1))
					(continue '())
				))))

				; We have now chosen room1 and room2.
				(rplac-assoc room1 (cons room2 (assoc room1 connections)) connections)
				(rplac-assoc room2 (cons room1 (assoc room2 connections)) connections)

				; Join the two "blobs" to which the two rooms belong, by modifying room labels.
				(set label1 (assoc room1 roomLabels))
				(set label2 (assoc room2 roomLabels))
				(set minLabel (if (< label1 label2) label1 label2))
				(set maxLabel (if (< label1 label2) label2 label1))
				(set roomLabels (mapcar (lambda (x)
					(if (= (cadr x) maxLabel) (list (car x) minLabel) x)
				) roomLabels))
				(set numberOfDifferentLabels (- numberOfDifferentLabels 1))
			))))
			(print "Labyrinth generation is complete.")
			(report)
			(printLongestPath)	; This sets roomGoal.
			(placeBooksInRooms) ; This uses roomGoal.
			(set JorgesRoom (nth (random (length rooms)) rooms))
			(print "") ; Print a blank line.
			(moveToRoom '(0 0))
		)
	)
))

; **** Application Commands ****

(set gen (lambda () (generate 15 7)))

(set move (lambda (n)
	(let ((adjacentRooms (assoc currentRoom connections)))
		(if (< n (length adjacentRooms))
			(moveToRoom (nth n adjacentRooms))
			(throw "Error: Input is out of range.")
		)
	)
))

(set help (lambda ()
	(findShortestPathBetweenRooms currentRoom roomGoal)
))

; End of File.
