      *****************************************************************
      * PROGRAM : ATMCLS
      * PURPOSE : 端末状態 (締め状態・連番の採番) の所有
      * DESIGN  :
      *   端末単位で永続する状態を持つ。他のマスタと同じく「ファイルを
      *   持つのは専用モジュール 1 つ」という規約に従う。締め状態の
      *   ファイルをバッチ本体が直接持つと、締めの制御と I/O の事情が
      *   混ざり、係員向けの解除ツールを作るときに再利用できない。
      *
      *   採番もここに置く。必要なのは「再起動をまたいで単調増加する
      *   識別子の発行者」であって、EJ の通番を覗いて代用すると、
      *   番号を 1 つ得るだけで EJ の追記経路が開いてしまう。
      *   永続状態を持つモジュールが発行するのが素直。
      *
      *   採番は払い出すたびに記録を更新する。覗くだけにすると、
      *   間に何も書かれなかった場合に同じ番号が二度出る。
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ATMCLS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CLOSE-FILE ASSIGN TO 'data/atmclose.dat'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS CLS-ATM-ID
               FILE STATUS IS WS-CLOSE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  CLOSE-FILE.
       COPY 'CLOSEREC.cpy'.

       WORKING-STORAGE SECTION.
       01  WS-CLOSE-STATUS             PIC X(02) VALUE '00'.
       01  WS-OPENED                   PIC X(01) VALUE 'N'.

       COPY 'ATMCONST.cpy'.
       COPY 'RETCODE.cpy'.

       LINKAGE SECTION.
       COPY 'CLSIF.cpy'.
       COPY 'ATMSESS.cpy'.

       PROCEDURE DIVISION USING CLS-PARM ATM-SESSION.

       MAIN-CONTROL SECTION.
       MAIN-START.
           MOVE RC-OK   TO CLS-OUT-RETCODE
           MOVE EC-NONE TO CLS-OUT-ERROR-CODE

           EVALUATE TRUE
               WHEN CLS-FN-CHECK    PERFORM CHECK-CLOSABLE
               WHEN CLS-FN-START    PERFORM START-CLOSING
               WHEN CLS-FN-FINISH   PERFORM FINISH-CLOSING
               WHEN CLS-FN-RELEASE  PERFORM RELEASE-FLAG
               WHEN CLS-FN-NEXT-NO  PERFORM ISSUE-NEXT-NO
               WHEN CLS-FN-CLOSE    PERFORM CLOSE-STATE-FILE
               WHEN OTHER
                   MOVE RC-FATAL TO CLS-OUT-RETCODE
           END-EVALUATE
           GOBACK.

       OPEN-STATE-FILE SECTION.
       OSF-START.
           IF WS-OPENED = 'Y'
               GO TO OSF-EXIT
           END-IF
           OPEN I-O CLOSE-FILE
           IF WS-CLOSE-STATUS = '00'
               MOVE 'Y' TO WS-OPENED
           ELSE
               MOVE RC-IO-ERROR  TO CLS-OUT-RETCODE
               MOVE EC-SYSTEM-IO TO CLS-OUT-ERROR-CODE
           END-IF.
       OSF-EXIT.
           EXIT.

       READ-STATE SECTION.
       RS-START.
           PERFORM OPEN-STATE-FILE
           IF CLS-OUT-RETCODE NOT = RC-OK
               GO TO RS-EXIT
           END-IF
           MOVE CN-ATM-ID TO CLS-ATM-ID
           READ CLOSE-FILE
               INVALID KEY
                   MOVE RC-NOTFOUND  TO CLS-OUT-RETCODE
                   MOVE EC-SYSTEM-IO TO CLS-OUT-ERROR-CODE
           END-READ
           MOVE CLS-LAST-CLOSED-DATE TO CLS-OUT-LAST-CLOSED.
       RS-EXIT.
           EXIT.

      *----------------------------------------------------------------
      * 締めてよいかの判定。状態は変えない。
      *   実行中フラグが残っている = 前回が異常終了した、を意味する。
      *   自動で解除すると多重実行を許すので、係員の解除を待つ。
      *----------------------------------------------------------------
       CHECK-CLOSABLE SECTION.
       CHK-START.
           PERFORM READ-STATE
           IF CLS-OUT-RETCODE NOT = RC-OK
               GO TO CHK-EXIT
           END-IF

           EVALUATE TRUE
               WHEN CLS-ST-RUNNING
                   MOVE RC-BUSINESS-ERROR   TO CLS-OUT-RETCODE
                   MOVE EC-CLOSE-IN-PROGRESS TO CLS-OUT-ERROR-CODE
               WHEN CLS-LAST-CLOSED-DATE >= SESS-BUSINESS-DATE
                   MOVE RC-BUSINESS-ERROR TO CLS-OUT-RETCODE
                   MOVE EC-ALREADY-CLOSED TO CLS-OUT-ERROR-CODE
           END-EVALUATE.
       CHK-EXIT.
           EXIT.

       START-CLOSING SECTION.
       SC-START.
           PERFORM READ-STATE
           IF CLS-OUT-RETCODE NOT = RC-OK
               GO TO SC-EXIT
           END-IF
           SET CLS-ST-RUNNING TO TRUE
           REWRITE CLOSE-RECORD
           END-REWRITE.
       SC-EXIT.
           EXIT.

       FINISH-CLOSING SECTION.
       FC-START.
           PERFORM READ-STATE
           IF CLS-OUT-RETCODE NOT = RC-OK
               GO TO FC-EXIT
           END-IF
           MOVE SESS-BUSINESS-DATE TO CLS-LAST-CLOSED-DATE
           MOVE SESS-TIMESTAMP     TO CLS-LAST-CLOSED-TS
           MOVE CLS-IN-DIFF-CNT    TO CLS-LAST-DIFF-CNT
           MOVE CLS-IN-PENDING-CNT TO CLS-LAST-PENDING-CNT
           SET  CLS-ST-IDLE TO TRUE
           REWRITE CLOSE-RECORD
           END-REWRITE.
       FC-EXIT.
           EXIT.

      *    -- 係員が原因を確認したあとに実行中フラグを落とす口。
       RELEASE-FLAG SECTION.
       RF-START.
           PERFORM READ-STATE
           IF CLS-OUT-RETCODE NOT = RC-OK
               GO TO RF-EXIT
           END-IF
           SET CLS-ST-IDLE TO TRUE
           REWRITE CLOSE-RECORD
           END-REWRITE.
       RF-EXIT.
           EXIT.

      *----------------------------------------------------------------
      * 連番の払出し。払い出すたびに記録を更新するので、間に何も
      * 起きなくても同じ番号は二度出ない。取引 ID とセッション ID の
      * 両方がこの系列から出る。用途の区別は呼出元の前置記号で表す。
      *----------------------------------------------------------------
       ISSUE-NEXT-NO SECTION.
       INN-START.
           PERFORM READ-STATE
           IF CLS-OUT-RETCODE NOT = RC-OK
               GO TO INN-EXIT
           END-IF
           ADD 1 TO CLS-LAST-SEQ
           MOVE CLS-LAST-SEQ TO CLS-OUT-NEXT-NO
           REWRITE CLOSE-RECORD
           END-REWRITE.
       INN-EXIT.
           EXIT.

       CLOSE-STATE-FILE SECTION.
       CSF-START.
           IF WS-OPENED = 'Y'
               CLOSE CLOSE-FILE
               MOVE 'N' TO WS-OPENED
           END-IF.
       CSF-EXIT.
           EXIT.

       END PROGRAM ATMCLS.
