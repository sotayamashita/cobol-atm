      *****************************************************************
      * PROGRAM : ATMLOAD
      * PURPOSE : カセット装填バッチ (係員が現金を補充する)
      * DESIGN  :
      *   端末が停止している時間帯に流す。オンライン中に走らせると
      *   取引が在庫を動かし続け、装填後の枚数が即座に意味を失う。
      *   日次締め (ATMDAY) と同じ前提である。
      *
      *   [装填を EJ に残す理由]
      *   装填は利用者の取引ではないが、現金が動く。残さないと、締めで
      *   検出した現金差異が装填によるものか、取引の不整合によるものかを
      *   区別できない。したがって取引と同じく S (開始) と E (終了) を
      *   書き、取引種別 'LD' で識別する。
      *
      *   [置換であって加算ではない]
      *   装填は「カセットを用意したものに差し替える」物理操作なので、
      *   装填後の枚数をそのまま設定する。補充額を入力させて足す形に
      *   すると、帳簿と現物がずれていた場合にずれが温存される。
      *
      *   [責務]
      *   在庫の書き換えは ATMCASH が持つ。このバッチが持つのは
      *   「何をどれだけ装填するか」の受け取りと可否判定だけである。
      *   金種の並びは払出アルゴリズムの前提 (降順) なので装填では
      *   変えられない。現在の金種をそのまま渡し、ATMCASH が一致を
      *   確かめる。
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ATMLOAD.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-WORK.
           05  WS-C                    PIC S9(04) COMP VALUE ZERO.
           05  WS-NEXT-NO              PIC 9(09) VALUE ZERO.
      *    -- 1 本でも装填するか。全部が「触らない」なら在庫も EJ も
      *    -- 動かさずに終える。何もしていない記録を残さない。
           05  WS-ANY-LOAD             PIC X(01) VALUE 'N'.
           05  WS-ABORT                PIC X(01) VALUE 'N'.

       01  WS-INPUT.
           05  WS-IN-LINE              PIC X(12) VALUE SPACES.
           05  WS-NUM-SIGNED           PIC S9(10) VALUE ZERO.

       01  WS-EDIT.
      *    -- 添字は COMP なので、そのまま DISPLAY すると符号が出る。
           05  WS-ED-CASSETTE          PIC 9(01).
           05  WS-ED-STATUS            PIC X(08).
           05  WS-ED-DENOM             PIC ZZ,ZZ9.
           05  WS-ED-NOTES             PIC ZZ,ZZ9.
           05  WS-ED-AMOUNT            PIC ---,---,---,--9.

       COPY 'ATMCONST.cpy'.
       COPY 'RETCODE.cpy'.
       COPY 'ATMSESS.cpy'.
       COPY 'CASHIF.cpy'.
       COPY 'JRNLIF.cpy'.
       COPY 'CLSIF.cpy'.

       01  WS-DATETIME.
           05  WS-CURRENT-DATE.
               10  WS-CD-YYYYMMDD      PIC 9(08).
               10  WS-CD-HHMMSS        PIC 9(06).
               10  FILLER              PIC X(15).

       PROCEDURE DIVISION.

       MAIN-CONTROL SECTION.
       MAIN-START.
           PERFORM INITIALIZE-BATCH
           IF WS-ABORT = 'Y'
               PERFORM TERMINATE-BATCH
               STOP RUN
           END-IF

           PERFORM SHOW-CASSETTES
           PERFORM ACCEPT-LOAD-PLAN

           IF WS-ANY-LOAD = 'N'
               DISPLAY ' '
               DISPLAY '  装填するカセットがありません。'
                       '在庫は変更していません。'
           ELSE
               PERFORM EXECUTE-LOAD
           END-IF

           PERFORM TERMINATE-BATCH
           STOP RUN.

      *================================================================
      * 初期化。EJ と現金機構を開く。
      *   現金機構を開けない場合は装填のしようがないので中止する。
      *================================================================
       INITIALIZE-BATCH SECTION.
       INIT-START.
      *    -- 共有域は必ず初期化してから使う。未初期化のままだと
      *    -- 英数字項目にバイナリゼロが残り、行順編成の EJ への
      *    -- 書込みが不正文字として拒否される。
           INITIALIZE ATM-SESSION
           MOVE CN-ATM-ID TO SESS-ATM-ID
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE
           MOVE WS-CD-YYYYMMDD TO SESS-BUSINESS-DATE
           COMPUTE SESS-TIMESTAMP =
               WS-CD-YYYYMMDD * 1000000 + WS-CD-HHMMSS

           DISPLAY ' '
           DISPLAY '=== カセット装填 端末 ' CN-ATM-ID
                   ' 営業日 ' SESS-BUSINESS-DATE ' ==='

           SET JRNL-FN-OPEN TO TRUE
           CALL 'ATMJRNL' USING JRNL-PARM ATM-SESSION

           SET CASH-FN-OPEN TO TRUE
           CALL 'ATMCASH' USING CASH-PARM ATM-SESSION
           IF CASH-OUT-RETCODE NOT = RC-OK
               DISPLAY '  [' CASH-OUT-ERROR-CODE
                       '] 現金機構に接続できません。'
               MOVE 'Y' TO WS-ABORT
               GO TO INIT-EXIT
           END-IF

           PERFORM NEXT-NO
           MOVE SPACES TO SESS-SESSION-ID
           STRING 'SES' DELIMITED BY SIZE
                  WS-NEXT-NO DELIMITED BY SIZE
               INTO SESS-SESSION-ID
           END-STRING.
       INIT-EXIT.
           EXIT.

       NEXT-NO SECTION.
       NN-START.
           SET CLS-FN-NEXT-NO TO TRUE
           CALL 'ATMCLS' USING CLS-PARM ATM-SESSION
           MOVE CLS-OUT-NEXT-NO TO WS-NEXT-NO.
       NN-EXIT.
           EXIT.

      *================================================================
      * 現在の帳簿枚数を示す。装填の要否は係員が現物を見て決めるので、
      * ここは判断材料を出すだけで可否は判定しない。
      *================================================================
       SHOW-CASSETTES SECTION.
       SHC-START.
           SET CASH-FN-THEORY TO TRUE
           CALL 'ATMCASH' USING CASH-PARM ATM-SESSION
           IF CASH-OUT-RETCODE NOT = RC-OK
               DISPLAY '  [' CASH-OUT-ERROR-CODE
                       '] 在庫を読めません。'
               MOVE 'Y' TO WS-ABORT
               GO TO SHC-EXIT
           END-IF

           DISPLAY ' '
           DISPLAY '  現在の帳簿枚数:'
           PERFORM VARYING WS-C FROM 1 BY 1 UNTIL WS-C > CN-CASSETTE-CNT
               MOVE WS-C                TO WS-ED-CASSETTE
               MOVE CASH-TH-DENOM(WS-C) TO WS-ED-DENOM
               MOVE CASH-TH-CNT(WS-C)   TO WS-ED-NOTES
               PERFORM EDIT-CASSETTE-STATUS
               DISPLAY '    カセット' WS-ED-CASSETTE ': ' WS-ED-DENOM
                       ' 円券 x ' WS-ED-NOTES ' 枚  ' WS-ED-STATUS
           END-PERFORM.
       SHC-EXIT.
           EXIT.

      *----------------------------------------------------------------
      * 状態を係員向けの文言にする。枚数だけでは障害中と正常が区別
      * できず、どのカセットを抜くべきか判断できない。装填の要否を
      * 決めるのは係員なので、ここは事実を示すだけにする。
      *----------------------------------------------------------------
       EDIT-CASSETTE-STATUS SECTION.
       ECS-START.
           EVALUATE TRUE
               WHEN CASH-TH-ST-FAULT(WS-C) MOVE '障害'   TO WS-ED-STATUS
               WHEN CASH-TH-ST-EMPTY(WS-C) MOVE '空'     TO WS-ED-STATUS
               WHEN CASH-TH-ST-LOW(WS-C)   MOVE '残少'   TO WS-ED-STATUS
               WHEN OTHER                  MOVE '正常'   TO WS-ED-STATUS
           END-EVALUATE.
       ECS-EXIT.
           EXIT.

      *================================================================
      * 装填内容の受け取り。カセットごとに装填後の枚数を入力させる。
      *   空入力は「このカセットは触らない」。ゼロは「空のカセットに
      *   差し替えた」であり、両者は意味が違うので区別して扱う。
      *================================================================
       ACCEPT-LOAD-PLAN SECTION.
       ALP-START.
           IF WS-ABORT = 'Y'
               GO TO ALP-EXIT
           END-IF

           DISPLAY ' '
           DISPLAY '  装填後の枚数を入力してください'
                   ' (空のまま Enter で変更しません)。'

           PERFORM VARYING WS-C FROM 1 BY 1 UNTIL WS-C > CN-CASSETTE-CNT
               MOVE SPACE TO CASH-LD-ACTION(WS-C)
               MOVE CASH-LO-DENOM(WS-C) TO CASH-LD-DENOM(WS-C)
               MOVE ZERO                TO CASH-LD-CNT(WS-C)

               MOVE CASH-LO-DENOM(WS-C) TO WS-ED-DENOM
               DISPLAY '    ' WS-ED-DENOM ' 円券の装填後枚数:'
      *        -- 入力前に必ず消す。入力が尽きた場合 ACCEPT は項目を
      *        -- 変えないため、前のカセットの入力が残る。
               MOVE SPACES TO WS-IN-LINE
               ACCEPT WS-IN-LINE

               IF WS-IN-LINE NOT = SPACES
                   PERFORM PARSE-COUNT
               END-IF
           END-PERFORM.
       ALP-EXIT.
           EXIT.

      *----------------------------------------------------------------
      * 入力された枚数の可否判定。装填バッチの責務はここまでで、
      * 在庫の書き換えは ATMCASH に任せる。
      *----------------------------------------------------------------
       PARSE-COUNT SECTION.
       PC-START.
      *    -- TEST-NUMVAL は数値として解釈できれば 0 を返す
           IF FUNCTION TEST-NUMVAL (WS-IN-LINE) NOT = ZERO
               DISPLAY '      枚数が正しくありません。このカセットは'
                       '変更しません。'
               GO TO PC-EXIT
           END-IF

      *    -- 符号付きで受けてから判定する。符号なし項目へ直接受けると、
      *    -- 負数が絶対値に化けて素通りする。
           COMPUTE WS-NUM-SIGNED = FUNCTION NUMVAL (WS-IN-LINE)
           IF WS-NUM-SIGNED < ZERO OR WS-NUM-SIGNED > 99999
               DISPLAY '      枚数が範囲外です。このカセットは'
                       '変更しません。'
               GO TO PC-EXIT
           END-IF

           MOVE WS-NUM-SIGNED TO CASH-LD-CNT(WS-C)
           SET CASH-LD-REPLACE(WS-C) TO TRUE
           MOVE 'Y' TO WS-ANY-LOAD.
       PC-EXIT.
           EXIT.

      *================================================================
      * 装填の実行。EJ(S) → 在庫更新 → EJ(E) の順は取引と同じ。
      *   先に S を書くのは、在庫更新の途中で落ちた場合に「装填を
      *   試みた」痕跡が残るようにするため。E が無ければ締めが
      *   不確定として拾う。
      *================================================================
       EXECUTE-LOAD SECTION.
       EXL-START.
           PERFORM NEXT-NO
           MOVE SPACES TO SESS-TXN-ID
           STRING 'T' DELIMITED BY SIZE
                  WS-NEXT-NO DELIMITED BY SIZE
               INTO SESS-TXN-ID
           END-STRING
           SET SESS-TT-CASH-LOAD TO TRUE
           MOVE ZERO    TO SESS-TXN-AMOUNT
           MOVE EC-NONE TO SESS-ERROR-CODE

           SET JRNL-PH-START   TO TRUE
           SET JRNL-RS-SUCCESS TO TRUE
           PERFORM WRITE-JRNL

           SET CASH-FN-LOAD TO TRUE
           CALL 'ATMCASH' USING CASH-PARM ATM-SESSION

           SET JRNL-PH-END TO TRUE
           IF CASH-OUT-RETCODE = RC-OK
               MOVE CASH-OUT-LOADED TO SESS-TXN-AMOUNT
               SET JRNL-RS-SUCCESS TO TRUE
               PERFORM SHOW-RESULT
           ELSE
               MOVE CASH-OUT-ERROR-CODE TO SESS-ERROR-CODE
               SET JRNL-RS-FAILED TO TRUE
               DISPLAY ' '
               DISPLAY '  [' CASH-OUT-ERROR-CODE
                       '] 装填できませんでした。在庫は変更していません。'
           END-IF
           PERFORM WRITE-JRNL.
       EXL-EXIT.
           EXIT.

       WRITE-JRNL SECTION.
       WJ-START.
           SET JRNL-FN-WRITE TO TRUE
           CALL 'ATMJRNL' USING JRNL-PARM ATM-SESSION.
       WJ-EXIT.
           EXIT.

       SHOW-RESULT SECTION.
       SR-START.
           DISPLAY ' '
           DISPLAY '  装填しました:'
           PERFORM VARYING WS-C FROM 1 BY 1 UNTIL WS-C > CN-CASSETTE-CNT
               IF CASH-LD-REPLACE(WS-C)
                   MOVE CASH-LD-DENOM(WS-C) TO WS-ED-DENOM
                   MOVE CASH-LD-CNT(WS-C)   TO WS-ED-NOTES
                   DISPLAY '    ' WS-ED-DENOM ' 円券 x ' WS-ED-NOTES
                           ' 枚'
               END-IF
           END-PERFORM
           MOVE CASH-OUT-LOADED TO WS-ED-AMOUNT
           DISPLAY '  在庫増減: ' WS-ED-AMOUNT ' 円'.
       SR-EXIT.
           EXIT.

       TERMINATE-BATCH SECTION.
       TB-START.
           SET CASH-FN-CLOSE TO TRUE
           CALL 'ATMCASH' USING CASH-PARM ATM-SESSION
           SET JRNL-FN-CLOSE TO TRUE
           CALL 'ATMJRNL' USING JRNL-PARM ATM-SESSION
           SET CLS-FN-CLOSE TO TRUE
           CALL 'ATMCLS' USING CLS-PARM ATM-SESSION.
       TB-EXIT.
           EXIT.

       END PROGRAM ATMLOAD.
