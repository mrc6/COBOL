       IDENTIFICATION DIVISION.
       PROGRAM-ID. STCTRL.
       AUTHOR. MARCOB.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQ-VSAM ASSIGN TO STCTRL
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS RANDOM
                  RECORD KEY IS FS-CODE
                  FILE STATUS IS WS-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  ARQ-VSAM.
       01 FS-PRODUCT-REG.
           05 FS-CODE        PIC X(10).
           05 FS-BARCODE PIC X(13).
           05 FS-NAME        PIC X(30).
           05 FS-DISCRIPTION PIC X(26).
           05 FS-CATEGORY    PIC X(10).
           05 FS-BRAND       PIC X(10).
           05 FS-UNIT        PIC X(5).
           05 FS-BUY-PRICE   PIC 9(08)V99 VALUE ZEROS.
           05 FS-SELL-PRICE  PIC 9(08)V99 VALUE ZEROS.
           05 FS-SUPPLIER    PIC X(30).
           05 FS-MIN-STORAGE PIC 9(08) VALUE ZEROS.
           05 FS-MAX-STORAGE PIC 9(08) VALUE ZEROS.

       WORKING-STORAGE SECTION.
         01 WS-PRODUCT.
           05 WS-CODE   PIC X(10) VALUE SPACES.
           05 WS-BARCODE PIC X(13) VALUE SPACES.
           05 WS-NAME        PIC X(30) VALUE SPACES.
           05 WS-DISCRIPTION PIC X(26) VALUE SPACES.
           05 WS-CATEGORY    PIC X(10) VALUE SPACES.
           05 WS-BRAND       PIC X(10) VALUE SPACES.
           05 WS-UNIT        PIC X(5) VALUE SPACES.
           05 WS-BUY-PRICE   PIC 9(08)V99 VALUE ZEROS.
           05 WS-SELL-PRICE  PIC 9(08)V99 VALUE ZEROS.
           05 WS-SUPPLIER    PIC X(30) VALUE SPACES.
           05 WS-MIN-STORAGE PIC 9(08) VALUE ZEROS.
           05 WS-MAX-STORAGE PIC 9(08) VALUE ZEROS.
         01  WS-STATUS               PIC XX.
           88  FS-OK           VALUE '00'.
         01 WS-OPTION        PIC 9(01) VALUE ZEROS.

       PROCEDURE DIVISION.
       MAIN.

      *    DISPLAY WS-PRODUCT.
           DISPLAY "============================================".
           DISPLAY "PRODUCT MENU".
           DISPLAY "============================================".
           DISPLAY "1 - CREATE PRODUCT REGISTER".
           DISPLAY "2 - SHOW PRODUCT REGISTER".
           DISPLAY "3 - MODIFY PRODUCT REGISTER".
           DISPLAY "4 - DELETE PRODUCT REGISTER".
           DISPLAY "5 - EXIT".
           DISPLAY "CHOOSE OPTION: ".
           DISPLAY "============================================".
           PERFORM MENU.

       MENU.
           ACCEPT WS-OPTION.
           DISPLAY "SELECTED OPTION: " WS-OPTION.

           EVALUATE WS-OPTION
               WHEN 1
                   PERFORM CREATE-PRODUCT
               WHEN 2
                   PERFORM SHOW-PRODUCT
               WHEN 3
                   PERFORM MODIFY-PRODUCT
               WHEN 4
                   PERFORM DELETE-PRODUCT
               WHEN 5
                    PERFORM END-PROGRAM
               WHEN OTHER
                   DISPLAY "INVALID OPTION, EXITING..."
               GOBACK
           END-EVALUATE.

       CREATE-PRODUCT.
           ACCEPT WS-CODE.
           ACCEPT WS-BARCODE.
           ACCEPT WS-NAME.
           ACCEPT WS-DISCRIPTION.
           ACCEPT WS-CATEGORY.
           ACCEPT WS-BRAND.
           ACCEPT WS-UNIT.
           ACCEPT WS-BUY-PRICE.
           ACCEPT WS-SELL-PRICE.
           ACCEPT WS-SUPPLIER.
           ACCEPT WS-MIN-STORAGE.
           ACCEPT WS-MAX-STORAGE.

           PERFORM OPEN-FILE.

           MOVE WS-CODE TO FS-CODE.
           MOVE WS-BARCODE TO FS-BARCODE.
           MOVE WS-NAME TO FS-NAME.
           MOVE WS-DISCRIPTION TO FS-DISCRIPTION.
           MOVE WS-CATEGORY TO FS-CATEGORY.
           MOVE WS-BRAND TO FS-BRAND.
           MOVE WS-UNIT TO FS-UNIT.
           MOVE WS-BUY-PRICE TO FS-BUY-PRICE.
           MOVE WS-SELL-PRICE TO FS-SELL-PRICE.
           MOVE WS-SUPPLIER TO FS-SUPPLIER.
           MOVE WS-MIN-STORAGE TO FS-MIN-STORAGE.
           MOVE WS-MAX-STORAGE TO FS-MAX-STORAGE.

           WRITE FS-PRODUCT-REG
               INVALID KEY DISPLAY 'ERROR: PRODUCT ID ALREADY EXISTS'
               NOT INVALID KEY
                   DISPLAY 'PRODUCT REGISTER CREATED SUCCESSFULLY'.
           PERFORM CLOSE-FILE.
           PERFORM END-PROGRAM.

       SHOW-PRODUCT.
           DISPLAY "SHOW PRODUCT MENU".

           ACCEPT WS-CODE.

           MOVE WS-CODE TO FS-CODE.
           PERFORM OPEN-FILE.
           READ ARQ-VSAM KEY IS FS-CODE
               INVALID KEY
                   DISPLAY "ERROR: PRODUCT ID NOT FOUND"
               NOT INVALID KEY
                   DISPLAY "PRODUCT-CODE: " FS-CODE.
                   DISPLAY "PRODUCT-BARCODE: " FS-BARCODE.
                   DISPLAY "PRODUCT-NAME: " FS-NAME.
                   DISPLAY "PRODUCT-DISCRIPTION: " FS-DISCRIPTION.
                   DISPLAY "PRODUCT-CATEGORY: " FS-CATEGORY.
                   DISPLAY "PRODUCT-BRAND: " FS-BRAND.
                   DISPLAY "PRODUCT-BUY-PRICE: " FS-BUY-PRICE.
                   DISPLAY "PRODUCT-SELL-PRICE: " FS-SELL-PRICE.
                   DISPLAY "PRODUCT-SUPPLIER: " FS-SUPPLIER.
                   DISPLAY "PRODUCT-MIN-STORAGE: " FS-MIN-STORAGE.
                   DISPLAY "PRODUCT-MAX-STORAGE: " FS-MAX-STORAGE.
           PERFORM CLOSE-FILE.
           PERFORM END-PROGRAM.

       MODIFY-PRODUCT.
           DISPLAY "MODIFY PRODUCT MENU".

           ACCEPT WS-CODE.

           MOVE WS-CODE TO FS-CODE.
           PERFORM OPEN-FILE.
           READ ARQ-VSAM KEY IS FS-CODE
               INVALID KEY
                   DISPLAY "ERROR: PRODUCT ID NOT FOUND"
               NOT INVALID KEY
                   DISPLAY "ENTER NEW PRODUCT VALUES: "
                   ACCEPT WS-CODE.
                   ACCEPT WS-BARCODE.
                   ACCEPT WS-NAME.
                   ACCEPT WS-DISCRIPTION.
                   ACCEPT WS-CATEGORY.
                   ACCEPT WS-BRAND.
                   ACCEPT WS-UNIT.
                   ACCEPT WS-BUY-PRICE.
                   ACCEPT WS-SELL-PRICE.
                   ACCEPT WS-SUPPLIER.
                   ACCEPT WS-MIN-STORAGE.
                   ACCEPT WS-MAX-STORAGE.

                   MOVE WS-CODE TO FS-CODE.
                   MOVE WS-BARCODE TO FS-BARCODE.
                   MOVE WS-NAME TO FS-NAME.
                   MOVE WS-DISCRIPTION TO FS-DISCRIPTION.
                   MOVE WS-CATEGORY TO FS-CATEGORY.
                   MOVE WS-BRAND TO FS-BRAND.
                   MOVE WS-UNIT TO FS-UNIT.
                   MOVE WS-BUY-PRICE TO FS-BUY-PRICE.
                   MOVE WS-SELL-PRICE TO FS-SELL-PRICE.
                   MOVE WS-SUPPLIER TO FS-SUPPLIER.
                   MOVE WS-MIN-STORAGE TO FS-MIN-STORAGE.
                   MOVE WS-MAX-STORAGE TO FS-MAX-STORAGE.

                   REWRITE FS-PRODUCT-REG
                   DISPLAY "PRODUCT REGISTER MODIFIED SUCCESSFULLY"

           PERFORM CLOSE-FILE.
           PERFORM END-PROGRAM.

       DELETE-PRODUCT.
           DISPLAY "DELETE PRODUCT MENU".
           ACCEPT WS-CODE.

           MOVE WS-CODE TO FS-CODE.

           PERFORM OPEN-FILE.

           READ ARQ-VSAM KEY IS FS-CODE
               INVALID KEY
                   DISPLAY "ERROR: PRODUCT ID NOT FOUND"
               NOT INVALID KEY
                   DELETE ARQ-VSAM
                   DISPLAY "PRODUCT REGISTER DELETED SUCCESSFULLY"
           END-READ.

           PERFORM CLOSE-FILE.
           PERFORM END-PROGRAM.

       OPEN-FILE.
           OPEN I-O ARQ-VSAM.
           IF WS-STATUS = "35"
               OPEN OUTPUT ARQ-VSAM
               CLOSE ARQ-VSAM
               OPEN I-O ARQ-VSAM
           END-IF.
           IF NOT FS-OK
               DISPLAY 'ERROR TO OPEN THE FILE. STATUS: ' WS-STATUS
               GOBACK
           END-IF.

       CLOSE-FILE.
           CLOSE ARQ-VSAM.

       END-PROGRAM.
           DISPLAY "FINISHED".
           GOBACK.
