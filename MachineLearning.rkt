; A Pixel is a [0-255]
; A NatNum between 0 and 255 representing a shade of grey.
(define PIXEL-WHITE 0)
(define PIXEL-BLACK 255)
(define PIXEL-GREY-L 60)
(define PIXEL-GREY-M 120)
(define PIXEL-GREY-D 180)

; Part b

; A PixelRow is one of:
; - empty
; - (cons Pixel PixelRow)
; And represents the rows of the grid representation of features/pixels to represent images

(define PIXELROW-0 empty)
(define PIXELROW-T1 (cons PIXEL-WHITE PIXELROW-0))
(define PIXELROW-T2 (cons PIXEL-BLACK PIXELROW-T1))
(define PIXELROW-TE (cons PIXEL-BLACK PIXELROW-T2))
(define PIXELROW-M1 (cons PIXEL-GREY-M PIXELROW-0))
(define PIXELROW-M2 (cons PIXEL-GREY-L PIXELROW-M1))
(define PIXELROW-ME (cons PIXEL-BLACK PIXELROW-M2))
(define PIXELROW-B1 (cons PIXEL-WHITE PIXELROW-0))
(define PIXELROW-B2 (cons PIXEL-GREY-D PIXELROW-B1))
(define PIXELROW-BE (cons PIXEL-GREY-L PIXELROW-B2))

(define (pixel-row-temp pr)
  (...
   (cond
     [(empty? pr) ...]
     [(cons? pr) (... (first pr) ...
                      (pixel-row-temp (rest pr)) ...)])))

; A Bitmap is one of:
; - empty
; (cons PixelRow Bitmap)
; And represents the full bitmap grid representation of an Image.
(define BITMAP-0 empty)
(define BITMAP-1 (cons PIXELROW-TE empty))
(define BITMAP-2 (cons PIXELROW-ME BITMAP-1))
(define BITMAP-3 (cons PIXELROW-BE BITMAP-2))

(define (bitmap-temp bm)
  (...
   (cond
     [(empty? bm) ...]
     [(cons? bm) (... (pixel-row-temp (first bm)) ...
                      (bitmap-temp (rest bm)) ...)])))

; Part c

; An Instance is one of:
; - empty
; - (cons Pixel Instance)
; And represents a flattened Bitmap

(define INSTANCE-0 empty)
(define INSTANCE-1 (cons PIXEL-GREY-D INSTANCE-0))
(define INSTANCE-2 (cons PIXEL-BLACK INSTANCE-1))
(define INSTANCE-3 (cons PIXEL-GREY-M INSTANCE-2))
(define INSTANCE-4 (cons PIXEL-BLACK INSTANCE-3))
(define INSTANCE-5 (cons PIXEL-WHITE INSTANCE-4))
(define INSTANCE-6 (cons PIXEL-BLACK INSTANCE-5))
(define INSTANCE-7 (cons PIXEL-GREY-L INSTANCE-6))
(define INSTANCE-8 (cons PIXEL-BLACK INSTANCE-7))
(define INSTANCE-9 (cons PIXEL-GREY-M INSTANCE-8))

(define (instance-temp i)
  (...
   (cond
     [(empty? i) ...]
     [(cons? i) (... (first i) ...
                     (instance-temp (rest i)) ...)])))

; Part d

(define BLACKDOT (square 10 "solid" "black"))
(define WHITEDOT (square 10 "solid" "white"))
(define GREYDOT (square 10 "solid" "grey"))

(define PIXELROW-BLACK (cons PIXEL-BLACK (cons PIXEL-BLACK (cons PIXEL-BLACK empty))))
(define PIXELROW-ZERO-M (cons PIXEL-BLACK (cons PIXEL-WHITE (cons PIXEL-BLACK empty))))
(define PIXELROW-ONE (cons PIXEL-WHITE (cons PIXEL-BLACK (cons PIXEL-WHITE empty))))
(define PIXELROW-SEVEN-B (cons PIXEL-BLACK (cons PIXEL-WHITE (cons PIXEL-WHITE empty))))

(define BITMAP-ZERO (cons PIXELROW-BLACK (cons PIXELROW-ZERO-M (cons PIXELROW-BLACK empty))))
(define BITMAP-ONE (cons PIXELROW-ONE (cons PIXELROW-ONE (cons PIXELROW-ONE empty))))
(define BITMAP-SEVEN (cons PIXELROW-BLACK (cons PIXELROW-ONE (cons PIXELROW-SEVEN-B empty))))

(define IMAGE-ONE (above
                   (beside WHITEDOT BLACKDOT WHITEDOT)
                   (beside WHITEDOT BLACKDOT WHITEDOT)
                   (beside WHITEDOT BLACKDOT WHITEDOT)))
(define IMAGE-ZERO (above
                    (beside BLACKDOT BLACKDOT BLACKDOT)
                    (beside BLACKDOT WHITEDOT BLACKDOT)
                    (beside BLACKDOT BLACKDOT BLACKDOT)))
(define IMAGE-SEVEN (above
                     (beside BLACKDOT BLACKDOT BLACKDOT)
                     (beside WHITEDOT WHITEDOT BLACKDOT)
                     (beside WHITEDOT WHITEDOT BLACKDOT)))

(define INSTANCE-ONE
  (cons PIXEL-BLACK
        (cons PIXEL-BLACK
              (cons PIXEL-BLACK
                    (cons PIXEL-BLACK
                          (cons PIXEL-WHITE
                                (cons PIXEL-BLACK
                                      (cons PIXEL-BLACK
                                            (cons PIXEL-BLACK
                                                  (cons PIXEL-BLACK empty))))))))))

(define INSTANCE-ZERO
  (cons PIXEL-WHITE
        (cons PIXEL-BLACK
              (cons PIXEL-WHITE
                    (cons PIXEL-WHITE
                          (cons PIXEL-BLACK
                                (cons PIXEL-WHITE
                                      (cons PIXEL-WHITE
                                            (cons PIXEL-BLACK
                                                  (cons PIXEL-WHITE empty))))))))))

(define INSTANCE-SEVEN
  (cons PIXEL-BLACK
        (cons PIXEL-BLACK
              (cons PIXEL-BLACK
                    (cons PIXEL-WHITE
                          (cons PIXEL-BLACK
                                (cons PIXEL-WHITE
                                      (cons PIXEL-BLACK
                                            (cons PIXEL-WHITE
                                                  (cons PIXEL-WHITE empty))))))))))

(define-struct training-image [bitmap visualization instance digit])
; A TrainingImage is a (make-training-image Bitmap Image Instance Digit)
; - where bitmap is a Bitmap representation of the TrainingImage's information,
; - visualization is a graphic depiction of the bitmap,
; - instance is an Instance representation of the TrainingImage's information,
; - and the Digit is the information that the aforementioned parameters are representing.
; The visualization and underlying data necessary for an image used to train machine learning.
(define TRAINING-ZERO (make-training-image BITMAP-ZERO IMAGE-ZERO INSTANCE-ZERO 0))
(define TRAINING-ONE (make-training-image BITMAP-ONE IMAGE-ONE INSTANCE-ONE 1))
(define TRAINING-SEVEN (make-training-image BITMAP-SEVEN IMAGE-SEVEN INSTANCE-SEVEN 7))

; training-image-temp : TrainingImage -> ???
(define (training-image-temp training)
  (... (bitmap-temp (training-image-bitmap training)) ...
       (training-image-visualization training) ...
       (instance-temp (training-image-instance training)) ...
       (training-image-digit training) ...))

(define-struct testing-image [bitmap visualization instance])
; A TestingImage is a (make-testing-image Bitmap Image Instance)
; - where bitmap is a Bitmap representation of the TestingImage's information,
; - visualization is a graphic depiction of the bitmap,
; - instance is an Instance representation of the TestingImage's information,
; The visualization and underlying data of an image used to test machine learning.
(define TESTING-ZERO (make-testing-image BITMAP-ZERO IMAGE-ZERO INSTANCE-ZERO))
(define TESTING-ONE (make-testing-image BITMAP-ONE IMAGE-ONE INSTANCE-ONE))
(define TESTING-SEVEN (make-testing-image BITMAP-SEVEN IMAGE-SEVEN INSTANCE-SEVEN))

; testing-image-temp : TestingImage -> ???
(define (testing-image-temp testing)
  (... (bitmap-temp (testing-image-bitmap testing)) ...
       (testing-image-visualization testing) ...
       (instance-temp (testing-image-instance testing)) ...))

; Part e

(define-struct neighbor [training distance])
; A Neighbor is a (make-neighbor Training NonNegNumber)
; Representing a Training image and a number that signifies the distance
; between the Training and the Testing image in question
(define NEIGHBOR-0-EXACT (make-neighbor TRAINING-ZERO 0))
(define NEIGHBOR-0-CLOSE (make-neighbor TRAINING-ZERO 100))
(define NEIGHBOR-1-EXACT (make-neighbor TRAINING-ONE 0))
(define NEIGHBOR-1-CLOSE (make-neighbor TRAINING-ONE 250.3))
(define NEIGHBOR-7-EXACT (make-neighbor TRAINING-SEVEN 0))
(define NEIGHBOR-7-CLOSE (make-neighbor TRAINING-SEVEN 168.5942))

(define (neighbor-temp n)
  (... (training-image-temp (neighbor-training n)) ...
       (neighbor-distance n) ...))

(define TESTLIST1 (list 0 10 0))
(define TESTLIST2 (list 0 5 0))
(define TESTLIST3 (list 4 6 8 10))
(define TESTLIST4 (list 2 4 6 8))


(define NEIGHBORLIST1 (list NEIGHBOR-0-EXACT NEIGHBOR-0-CLOSE))
(define NEIGHBORLIST2 (list NEIGHBOR-7-EXACT NEIGHBOR-7-CLOSE))

(define TV1 (testing-image-visualization TESTING-ZERO))
(define TV7 (testing-image-visualization TESTING-SEVEN))

; mnist: Nat String --> Nat
; Displays a visualization of all the training images as well as the closest neighbor
; and the testing image and returns the digit of the closest neighbor

(define (mnist num tfname)
  (local [(define NEIGHBORS-LIST (create-neighbors num tfname))
          (define CLOSEST-NEIGHBOR (nearest-neighbor num tfname))
          (define TESTING-IMAGE (bitmap->image (read-lolon tfname)))]
    (return-former (training-image-digit (neighbor-training CLOSEST-NEIGHBOR))
                   (big-bang 0
                     [to-draw (λ(num) (draw-image num NEIGHBORS-LIST CLOSEST-NEIGHBOR TESTING-IMAGE))]
                     [on-key (λ(num key) (update-index num key NEIGHBORS-LIST))]))))

(define BACKGROUND (square 800 "solid" "white"))

; create-neigbhors: Nat String --> [List-of Neighbor]
; Creates a [List-of Neighbor] given a number and the filename of a testing image

(check-expect (create-neighbors 1 "test/d_55555_9.txt")
              (map (λ(tr) (make-neighbor tr
                                         (euclidean-distance
                                          squared-diff
                                          (training-image-instance tr)
                                          (flatten (read-lolon "test/d_55555_9.txt")))))
                   (create-training-image 1)))

(check-expect (create-neighbors 2 "test/d_28123_4.txt")
              (map (λ(tr) (make-neighbor tr
                                         (euclidean-distance
                                          squared-diff
                                          (training-image-instance tr)
                                          (flatten (read-lolon "test/d_28123_4.txt")))))
                   (create-training-image 2)))



(define (create-neighbors num test)
  (map (λ(tr) (make-neighbor tr
                             (euclidean-distance
                              squared-diff
                              (training-image-instance tr)
                              (flatten (read-lolon test)))))
       (create-training-image num)))

; euclidean-distance: [Number Number --> Number] [List-of Number] [List-of Number] --> Number
; Produces the eculidean distance between two lists of numbers of equal length

(check-expect (euclidean-distance squared-diff TESTLIST1 TESTLIST2) 5)
(check-expect (euclidean-distance squared-diff TESTLIST3 TESTLIST4) 4)
              
(define (euclidean-distance dist l1 l2)
  (inexact->exact(sqrt (foldr + 0 (map-2list squared-diff l1 l2)))))

; map-2list : (X) String [List-of X] [List-of X] -> [List-of X]
; Applies a function to elements with the same index in same sized lists

(check-expect (map-2list + (list 1 2) (list 3 4)) (list 4 6))
(check-expect (map-2list string-append (list "He" "the") (list "llo" "re"))(list "Hello" "there"))
(check-expect (map-2list + empty (list 3 4)) empty)
(check-error (map-2list + (list 1 2 3) (list 3 4)) "Lists are not the same size")

(define (map-2list operator l1 l2)
  (cond
    [(or (empty? l1) (empty? l2)) empty]
    [(not (= (length l1) (length l2))) (error "Lists are not the same size")]
    [(and (cons? l1) (cons? l2)) (append (cons (operator (first l1) (first l2))
                                               (map-2list operator (rest l1) (rest l2))))]))

; squared-diff: Number Number --> RealNumber
; Produces the squared value of the difference of two numbers

(check-expect (squared-diff 0 0) 0)
(check-expect (squared-diff 2 0) 4)
(check-expect (squared-diff 3 -1) 16)

(define (squared-diff x y)
  (sqr (- x y)))

; flatten: (X) [List-of [List-of X]] --> [List-of X]
; Flattens a a list of lists to a list

(check-expect (flatten (list (list 1 2) (list 3 4))) (list 1 2 3 4))
(check-expect (flatten (list (list "Our names are")
                             (list  "Ritik " "and " "Genesis")))
              (list "Our names are" "Ritik " "and " "Genesis"))

(define (flatten l)
  (foldr append empty l))

; create-training-image: Nat --> [List-of TrainingImage]
; Produces a [List-of TrainingImage] based off of a given number that generates the filenames

(check-expect (create-training-image 1)
              (map (λ(fn) (make-training-image (read-lolon fn)
                                               (bitmap->image (read-lolon fn))
                                               (flatten (read-lolon fn))
                                               (fname->label fn)))
                   (training-fnames 1)))

(check-expect (create-training-image 3)
              (map (λ(fn) (make-training-image (read-lolon fn)
                                               (bitmap->image (read-lolon fn))
                                               (flatten (read-lolon fn))
                                               (fname->label fn)))
                   (training-fnames 3)))

(define (create-training-image num)
  (map (λ(fn) (make-training-image (read-lolon fn)
                                   (bitmap->image (read-lolon fn))
                                   (flatten (read-lolon fn))
                                   (fname->label fn)))
       (training-fnames num)))

; read-lolon : String -> [List-of [List-of Number]]
; Produces a list of lists of numbers, where each inner list is a row in the file,
; given a file that contains space-separated numbers
(write-file "numbers.txt" "0 0 0 0\n1 2 3 4\n2 1 0 -0.5")
(write-file "numbers2.txt" "21    1    2\n1 4  5\n 6  3")

(check-expect (read-lolon "numbers.txt")
              (list
               (list 0 0 0 0)
               (list 1 2 3 4)
               (list 2 1 0 -0.5)))

(check-expect (read-lolon "numbers2.txt")
              (list
               (list 21 1 2)
               (list 1 4 5)
               (list 6 3)))

(define (read-lolon filename)
  (map-lol string->number (read-words/line filename)))

; map-lol: (X Y) [X --> Y] [List-of [List-of X]] --> [List-of [List-of Y]]
; maps a list of lists of one type to another using a function

(check-expect (map-lol
               string->number
               (list
                (list "0" "0" "0")
                (list "1" "2" "3")
                (list "3" "2" "1")))
              (list
               (list 0 0 0)
               (list 1 2 3)
               (list 3 2 1)))

(check-expect (map-lol
               number->string
               (list
                (list 0 0 0)
                (list 1 2 3)))
              (list
               (list "0" "0" "0")
               (list "1" "2" "3")))

(check-expect (map-lol
               string-length
               (list
                (list "hi" "hello" "bye")
                (list "This" "is" "a" "test")))
              (list
               (list 2 5 3)
               (list 4 2 1 4)))
               
                     

(define (map-lol converter lol)
  (map (λ(l) (map converter l)) lol))
                               
; bitmap->image: Bitmap --> Image
; produces a visualization of a grid of features or pixels.

(check-expect (bitmap->image (list
                              (list 200 255 205)
                              (list 255 0   213)
                              (list 252 255 105)))
              (above
               (beside (square 10 "solid" (make-color 55 55 55))
                       (square 10 "solid" (make-color 0 0 0))
                       (square 10 "solid" (make-color 50 50 50)))
               (beside (square 10 "solid" (make-color 0 0 0))
                       (square 10 "solid" (make-color 255 255 255))
                       (square 10 "solid" (make-color 42 42 42)))
               (beside (square 10 "solid" (make-color 3 3 3))
                       (square 10 "solid" (make-color 0 0 0))
                       (square 10 "solid" (make-color 150 150 150)))))

(check-expect (bitmap->image (list
                              (list 240 255 230)
                              (list 20 5 254)
                              (list 20 5 255)))
              (above
               (beside (square 10 "solid" (make-color 15 15 15))
                       (square 10 "solid" (make-color 0 0 0))
                       (square 10 "solid" (make-color 25 25 25)))
               (beside (square 10 "solid" (make-color 235 235 235))
                       (square 10 "solid" (make-color 250 250 250))
                       (square 10 "solid" (make-color 1 1 1)))
               (beside (square 10 "solid" (make-color 235 235 235))
                       (square 10 "solid" (make-color 250 250 250))
                       (square 10 "solid" (make-color 0 0 0)))))
                       

(define (bitmap->image bitmap)
  (foldr above empty-image (map draw-row (map-lol create-square bitmap))))


; draw-row: [List-of-Image] --> Image
; Creates a row of the visualization given a list of images

(check-expect (draw-row (list (square 10 "solid" (make-color 15 15 15))
                              (square 10 "solid" (make-color 0 0 0))
                              (square 10 "solid" (make-color 25 25 25))))
              (beside (square 10 "solid" (make-color 15 15 15))
                      (square 10 "solid" (make-color 0 0 0))
                      (square 10 "solid" (make-color 25 25 25))))

(check-expect (draw-row (list (square 10 "solid" (make-color 235 235 235))
                              (square 10 "solid" (make-color 250 250 250))
                              (square 10 "solid" (make-color 1 1 1))))
              (beside (square 10 "solid" (make-color 235 235 235))
                      (square 10 "solid" (make-color 250 250 250))
                      (square 10 "solid" (make-color 1 1 1))))
              
                     
(define (draw-row losq)
  (foldr beside empty-image losq))

; create-square: Pixel --> Image
; Creates each square in the visualization given a pixel to determine color

(check-expect (create-square 200) (square 10 "solid" (make-color 55 55 55)))
(check-expect (create-square 55) (square 10 "solid" (make-color 200 200 200)))
(check-expect (create-square 0) (square 10 "solid" (make-color 255 255 255)))

(define (create-square px)
  (square 10 "solid" (make-color (- 255 px) (- 255 px) (- 255 px))))

; fname->label : String -> Nat
; Outputs integer in a file name that ends with #.??? , where # is a natural number

(check-expect (fname->label "hello2.jpg") 2)
(check-expect (fname->label "num0.pdf") 0)
(check-expect (fname->label "InsertRandomFileNameHere4.txt") 4)

(define (fname->label filename)
  (local [(define sl (string-length filename))]
    (string->number(substring filename (- sl 5) (- sl 4)))))


; training-fnames: Nat --> [List-of String]
; produces a list of file paths based on the inputted natural number

(check-expect (training-fnames 0) empty)

(check-expect (training-fnames 2) (list
                                   "train/d_1_0.txt" "train/d_2_0.txt"
                                   "train/d_1_1.txt" "train/d_2_1.txt"
                                   "train/d_1_2.txt" "train/d_2_2.txt"
                                   "train/d_1_3.txt" "train/d_2_3.txt"
                                   "train/d_1_4.txt" "train/d_2_4.txt"
                                   "train/d_1_5.txt" "train/d_2_5.txt"
                                   "train/d_1_6.txt" "train/d_2_6.txt"
                                   "train/d_1_7.txt" "train/d_2_7.txt"
                                   "train/d_1_8.txt" "train/d_2_8.txt"
                                   "train/d_1_9.txt" "train/d_2_9.txt"))

(check-expect
 (training-fnames 3)
 (list "train/d_1_0.txt" "train/d_2_0.txt" "train/d_3_0.txt"
       "train/d_1_1.txt" "train/d_2_1.txt" "train/d_3_1.txt"
       "train/d_1_2.txt" "train/d_2_2.txt" "train/d_3_2.txt"
       "train/d_1_3.txt" "train/d_2_3.txt" "train/d_3_3.txt"
       "train/d_1_4.txt" "train/d_2_4.txt" "train/d_3_4.txt"
       "train/d_1_5.txt" "train/d_2_5.txt" "train/d_3_5.txt"
       "train/d_1_6.txt" "train/d_2_6.txt" "train/d_3_6.txt"
       "train/d_1_7.txt" "train/d_2_7.txt" "train/d_3_7.txt"
       "train/d_1_8.txt" "train/d_2_8.txt" "train/d_3_8.txt"
       "train/d_1_9.txt" "train/d_2_9.txt" "train/d_3_9.txt"))


(define (training-fnames num1)
  (flatten (build-list 10 (λ(num2) (generate-fnames num1 num2)))))

; generate-fnames: Nat Nat --> [List-of String]
; generates a single list of filenames with the number of the file name being the second inputted
; number and the length of the list being the first inputted number

(check-expect (generate-fnames 0 0) empty)
(check-expect (generate-fnames 1 2) (list "train/d_1_2.txt"))
(check-expect (generate-fnames 3 4) (list "train/d_1_4.txt" "train/d_2_4.txt" "train/d_3_4.txt"))

(define (generate-fnames num1 num2)
  (build-list num1  (λ(num1) (create-fname num1 num2))))

; create-fname: Nat Nat --> String
; Produces the file name based off of the pattern using the two natural numbers supplied

(check-expect (create-fname 0 0) "train/d_1_0.txt")
(check-expect (create-fname 1 2) "train/d_2_2.txt")
(check-expect (create-fname 4 5) "train/d_5_5.txt")

(define (create-fname num1 num2)
  (string-append "train/d_" (number->string (+ num1 1)) "_" (number->string num2) ".txt"))

; nearest-neighbor: Nat String --> Neighbor
; Produces the nearest neighbor based off of the distance given the filename of the testing image
; and a number

(check-expect (nearest-neighbor 3 "test/d_55555_9.txt")
              (smallest-of-list-by-f neighbor-distance (create-neighbors 3 "test/d_55555_9.txt")))

(check-expect (nearest-neighbor 2 "test/d_48201_8.txt")
              (smallest-of-list-by-f neighbor-distance (create-neighbors 2 "test/d_48201_8.txt")))
              
(define (nearest-neighbor num test)
  (smallest-of-list-by-f neighbor-distance (create-neighbors num test)))

; smallest-of-list-by-f: (X) [X --> Number] [List-of X] --> X
; finds the first element in a non-empty list that minimizes a supplied function

(check-expect
 (smallest-of-list-by-f length
                        (list
                         (list 1 2 3)
                         (list 100)
                         (list -1000 -99 -1 0)
                         (list 2))) (list 100))

(check-expect
 (smallest-of-list-by-f string-length
                        (list
                         "hi" "hello" "a" "c" "byebye")) "a")


(check-expect
 (smallest-of-list-by-f length
                        (list
                         (list (make-posn 1 1)))) (list (make-posn 1 1)))


(define (smallest-of-list-by-f comparator lol)
  (first (sort lol (λ(a b) (< (comparator a) (comparator b))))))

; return-former: (X Y) X Y --> X
; Takes in two values and returns the value of the first

(check-expect (return-former #true +) #true)
(check-expect (return-former "1" #false) "1")
(check-expect (return-former (make-posn 1 2) (make-posn 3 4)) (make-posn 1 2))


(define (return-former x y)
  x)

; draw-image: Nat [List-of Neighbor] Neighbor TestingImage --> Image
; Displays the current index in the [List-of Neighbor] along with the closest Neighbor and the
; TestingImage

(check-expect (draw-image 0 NEIGHBORLIST1 NEIGHBOR-0-EXACT TV1)
              (above/align "left" (text "Test Image" 20 "red")
                           TV1
                           (text
                            (string-append
                             "Best Match: "
                             (number->string
                              (training-image-digit(neighbor-training NEIGHBOR-0-EXACT))))
                            20 "purple")
                           (text
                            (number->string (exact->inexact(neighbor-distance NEIGHBOR-0-EXACT)))
                            20 "purple")
                           (beside
                            (training-image-visualization (neighbor-training NEIGHBOR-0-EXACT))
                            (above/align "middle"
                                         (text "Training" 20 "blue")
                                         (text (string-append
                                                (number->string
                                                 (training-image-digit
                                                  (neighbor-training (list-ref NEIGHBORLIST1 0))))
                                                ": "
                                                (number->string
                                                 (exact->inexact
                                                  (neighbor-distance (list-ref NEIGHBORLIST1 0)))))
                                               20 "blue"))
                            (training-image-visualization
                             (neighbor-training (list-ref NEIGHBORLIST1 0))))
                           BACKGROUND))

(check-expect (draw-image 1 NEIGHBORLIST2 NEIGHBOR-7-EXACT TV7)
              (above/align "left" (text "Test Image" 20 "red")
                           TV7
                           (text
                            (string-append
                             "Best Match: "
                             (number->string
                              (training-image-digit(neighbor-training NEIGHBOR-7-EXACT))))
                            20 "purple")
                           (text
                            (number->string (exact->inexact(neighbor-distance NEIGHBOR-7-EXACT)))
                            20 "purple")
                           (beside
                            (training-image-visualization (neighbor-training NEIGHBOR-7-EXACT))
                            (above/align "middle"
                                         (text "Training" 20 "blue")
                                         (text (string-append
                                                (number->string
                                                 (training-image-digit
                                                  (neighbor-training (list-ref NEIGHBORLIST2 1))))
                                                ": "
                                                (number->string
                                                 (exact->inexact
                                                  (neighbor-distance (list-ref NEIGHBORLIST2 1)))))
                                               20 "blue"))
                            (training-image-visualization
                             (neighbor-training (list-ref NEIGHBORLIST2 1))))
                           BACKGROUND))

(define (draw-image n nlist cneighbor timage)
  (above/align "left" (text "Test Image" 20 "red")
               timage
               (text
                (string-append
                 "Best Match: "(number->string(training-image-digit (neighbor-training cneighbor))))
                20 "purple")
               (text (number->string (exact->inexact(neighbor-distance cneighbor))) 20 "purple")
               (beside
                (training-image-visualization (neighbor-training cneighbor))
                (above/align "middle"
                             (text "Training" 20 "blue")
                             (text (string-append
                                    (number->string(training-image-digit
                                                    (neighbor-training (list-ref nlist n))))
                                    ": "
                                    (number->string (exact->inexact
                                                     (neighbor-distance (list-ref nlist n)))))
                                   20 "blue"))
                (training-image-visualization (neighbor-training (list-ref nlist n))))
               BACKGROUND)) 

; update-index: Nat KeyEvent [List-of Neighbor] --> Nat
; Returns the updated index based off the user's key input

(check-expect (update-index 0 "left" NEIGHBORLIST1) (prev-index NEIGHBORLIST1 0))
(check-expect (update-index 1 "right" NEIGHBORLIST2) (next-index NEIGHBORLIST2 1))

(define (update-index n key nlist)
  (cond
    [(string=? key "right") (next-index nlist n)]
    [(string=? key "left") (prev-index nlist n)]
    [else n]))

; next-index: (X) [List-of X] Nat --> Nat
; returns the next valid index (or 0 if the supplied index is the last valid index).

(check-expect (next-index (list 1 2 3) 0) 1)
(check-expect (next-index (list 1 2 3) 1) 2)
(check-expect (next-index (list 1 2 3) 2) 0)


(define (next-index l index)
  (cond
    [(= index (- (length l) 1)) 0]
    [else (+ index 1)]))

; prev-index: (X) [List-of X] Nat --> Nat
; returns the valid index before that which was supplied
; (or the highest valid index, if 0 was supplied

(check-expect (prev-index (list 1 2 3) 0) 2)
(check-expect (prev-index (list 1 2 3) 1) 0)
(check-expect (prev-index (list 1 2 3) 2) 1)

(define (prev-index l index)
  (cond
    [(= index 0) (- (length l) 1)]
    [else (- index 1)]))
  

(mnist 5 "test/d_35001_5.txt")
