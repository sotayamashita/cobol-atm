      *****************************************************************
      * PROGRAM : ATMMAIN
      * PURPOSE : ATM 端末制御 (セッション管理・画面遷移・取引順序制御)
      * DESIGN  :
      *   画面は 1 系統のコンソールのみとする。ただし取引の順序制御は
      *   実機と同じ規律を守る。特に重要なのは以下の 2 点。
      *
      *   [出金]  EJ(START) → 金種計画 → 記帳 → 繰出 → EJ(END)
      *           金種計画を記帳より前に行うのは、記帳成立後に
      *           「紙幣が出せない」状態を作らないため。
      *           繰出が失敗した場合のみ記帳を取り消す (REVERSE)。
      *
      *   [入金]  紙幣は一時保留部に収納済みとみなす。
      *           EJ(START) → 記帳 → 金庫収納 → EJ(END)
      *           収納に失敗した場合は紙幣を返却し記帳を取り消す。
      *
      *   セッション ID は 1 カード投入につき 1 つ。取引 ID は取引ごと。
      *   EJ にはこの 2 つを常に出力し、事後の追跡を可能にする。
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ATMMAIN.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY 'ATMCONST.cpy'.

       01  WS-CONST.
           05  WS-MAX-PIN-RETRY        PIC 9(01) VALUE 3.

       01  WS-CTRL.
           05  WS-SESSION-SEQ          PIC 9(06) VALUE ZERO.
           05  WS-TXN-SEQ              PIC 9(06) VALUE ZERO.
           05  WS-TERMINATE            PIC X(01) VALUE 'N'.
           05  WS-RETRY                PIC 9(01) VALUE ZERO.
           05  WS-MENU                 PIC X(01) VALUE SPACE.

       01  WS-INPUT.
           05  WS-IN-PAN               PIC X(16) VALUE SPACES.
           05  WS-IN-PIN               PIC X(04) VALUE SPACES.
           05  WS-IN-NEW-PIN           PIC X(04) VALUE SPACES.
           05  WS-IN-AMOUNT            PIC X(12) VALUE SPACES.
           05  WS-IN-ACCT              PIC X(10) VALUE SPACES.
           05  WS-NUM-AMOUNT           PIC 9(10) VALUE ZERO.

       01  WS-DATETIME.
           05  WS-CURRENT-DATE.
               10  WS-CD-YYYYMMDD      PIC 9(08).
               10  WS-CD-HHMMSS        PIC 9(06).
               10  FILLER              PIC X(15).

       01  WS-EDIT.
           05  WS-ED-AMOUNT            PIC ---,---,---,--9.
           05  WS-ED-NOTES             PIC ZZ9.
           05  WS-ED-DENOM             PIC ZZ,ZZ9.
           05  WS-I                    PIC S9(04) COMP VALUE ZERO.

       COPY 'RETCODE.cpy'.
       COPY 'ATMSESS.cpy'.
       COPY 'AUTHIF.cpy'.
       COPY 'POSTIF.cpy'.
       COPY 'CASHIF.cpy'.
       COPY 'JRNLIF.cpy'.

       PROCEDURE DIVISION.

       MAIN-CONTROL SECTION.
       MAIN-START.
           PERFORM INITIALIZE-TERMINAL
           PERFORM UNTIL WS-TERMINATE = 'Y'
               PERFORM CARD-SESSION
           END-PERFORM
           PERFORM TERMINATE-TERMINAL
           STOP RUN.

       INITIALIZE-TERMINAL SECTION.
       INIT-START.
           MOVE CN-ATM-ID TO SESS-ATM-ID
           PERFORM REFRESH-CLOCK

           SET JRNL-FN-OPEN TO TRUE
           CALL 'ATMJRNL' USING JRNL-PARM ATM-SESSION

           SET CASH-FN-OPEN TO TRUE
           CALL 'ATMCASH' USING CASH-PARM ATM-SESSION
           IF CASH-OUT-RETCODE NOT = RC-OK
               DISPLAY '*** 現金機構に接続できません。取扱を中止します'
               MOVE 'Y' TO WS-TERMINATE
           END-IF

           DISPLAY ' '
           DISPLAY '================================================'
           DISPLAY '   COBOL ATM  端末 ' CN-ATM-ID '  店番 '
                   CN-BRANCH-CD
           DISPLAY '================================================'.
       INIT-EXIT.
           EXIT.

       TERMINATE-TERMINAL SECTION.
       TERM-START.
           SET POST-FN-CLOSE TO TRUE
           CALL 'ATMPOST' USING POST-PARM ATM-SESSION
           SET AUTH-FN-CLOSE TO TRUE
           CALL 'ATMAUTH' USING AUTH-PARM ATM-SESSION
           SET CASH-FN-CLOSE TO TRUE
           CALL 'ATMCASH' USING CASH-PARM ATM-SESSION
           SET JRNL-FN-CLOSE TO TRUE
           CALL 'ATMJRNL' USING JRNL-PARM ATM-SESSION
           DISPLAY 'ご利用ありがとうございました。'.
       TERM-EXIT.
           EXIT.

      *================================================================
      * カード 1 枚分のセッション
      *================================================================
       CARD-SESSION SECTION.
       CS-START.
           DISPLAY ' '
           DISPLAY 'カード番号を入力してください (空 Enter で終了):'
           ACCEPT WS-IN-PAN
           IF WS-IN-PAN = SPACES
               MOVE 'Y' TO WS-TERMINATE
               GO TO CS-EXIT
           END-IF

           PERFORM START-SESSION
           PERFORM AUTHENTICATE
           IF SESS-AUTH-NG
               PERFORM END-SESSION
               GO TO CS-EXIT
           END-IF

           DISPLAY ' '
           DISPLAY FUNCTION TRIM (SESS-HOLDER-NAME) ' 様'
           MOVE 'N' TO WS-MENU
           PERFORM UNTIL WS-MENU = '9'
               PERFORM SHOW-MENU
               PERFORM DISPATCH-TRANSACTION
           END-PERFORM

           PERFORM END-SESSION.
       CS-EXIT.
           EXIT.

       START-SESSION SECTION.
       SS-START.
           ADD 1 TO WS-SESSION-SEQ
           PERFORM REFRESH-CLOCK
           MOVE SPACES TO SESS-SESSION-ID
           STRING CN-ATM-ID DELIMITED BY SIZE
                  WS-SESSION-SEQ DELIMITED BY SIZE
               INTO SESS-SESSION-ID
           END-STRING
           MOVE 'N'   TO SESS-AUTHENTICATED
           MOVE ZERO  TO SESS-TXN-AMOUNT SESS-TXN-FEE
           MOVE SPACES TO SESS-CPTY-ACCT-NO.
       SS-EXIT.
           EXIT.

       END-SESSION SECTION.
       ES-START.
           SET AUTH-FN-EJECT TO TRUE
           CALL 'ATMAUTH' USING AUTH-PARM ATM-SESSION
           DISPLAY 'カードを排出しました。'.
       ES-EXIT.
           EXIT.

      *----------------------------------------------------------------
      * 認証。最大 3 回。閉塞指示が返ったらカードを取り込む。
      *----------------------------------------------------------------
       AUTHENTICATE SECTION.
       AU-START.
           MOVE ZERO TO WS-RETRY
           PERFORM UNTIL WS-RETRY >= WS-MAX-PIN-RETRY
                      OR SESS-AUTH-OK
               DISPLAY '暗証番号 (4 桁) を入力してください:'
               ACCEPT WS-IN-PIN
               PERFORM REFRESH-CLOCK

               SET AUTH-FN-VERIFY TO TRUE
               MOVE WS-IN-PAN      TO AUTH-IN-PAN
               MOVE WS-IN-PIN      TO AUTH-IN-PIN
               CALL 'ATMAUTH' USING AUTH-PARM ATM-SESSION

               IF AUTH-OUT-RETCODE = RC-OK
                   EXIT PERFORM
               END-IF

               MOVE AUTH-OUT-ERROR-CODE TO SESS-ERROR-CODE
               PERFORM SHOW-ERROR
               IF AUTH-OUT-CARD-CAPTURED = 'Y'
                   DISPLAY '*** カードをお預かりしました。'
                           '窓口へお申し出ください。'
                   MOVE WS-MAX-PIN-RETRY TO WS-RETRY
                   EXIT PERFORM
               END-IF
      *        -- 存在しないカードは再入力させず即終了 (総当たり対策)
               IF AUTH-OUT-RETCODE = RC-NOTFOUND
                   MOVE WS-MAX-PIN-RETRY TO WS-RETRY
                   EXIT PERFORM
               END-IF
               ADD 1 TO WS-RETRY
           END-PERFORM.
       AU-EXIT.
           EXIT.

       SHOW-MENU SECTION.
       SM-START.
           DISPLAY ' '
           DISPLAY '--- お取引をお選びください -------------------'
           DISPLAY '  1: 残高照会   2: お引出し   3: お預入れ'
           DISPLAY '  4: お振込み   5: 暗証番号変更   9: 終了'
           ACCEPT WS-MENU.
       SM-EXIT.
           EXIT.

       DISPATCH-TRANSACTION SECTION.
       DT-START.
           EVALUATE WS-MENU
               WHEN '1' PERFORM TXN-INQUIRY
               WHEN '2' PERFORM TXN-WITHDRAW
               WHEN '3' PERFORM TXN-DEPOSIT
               WHEN '4' PERFORM TXN-TRANSFER
               WHEN '5' PERFORM TXN-CHANGE-PIN
               WHEN '9' CONTINUE
               WHEN OTHER
                   DISPLAY '選択が正しくありません。'
           END-EVALUATE.
       DT-EXIT.
           EXIT.

      *================================================================
      * 残高照会
      *================================================================
       TXN-INQUIRY SECTION.
       TI-START.
           PERFORM START-TRANSACTION
           SET SESS-TT-INQUIRY    TO TRUE
           MOVE ZERO  TO SESS-TXN-AMOUNT
           PERFORM WRITE-JRNL-START

           SET POST-FN-INQUIRY TO TRUE
           MOVE ZERO            TO POST-IN-AMOUNT
           CALL 'ATMPOST' USING POST-PARM ATM-SESSION
           PERFORM COPY-POST-RESULT

           IF POST-OUT-RETCODE = RC-OK
               DISPLAY ' '
               MOVE POST-OUT-BAL-AFTER TO WS-ED-AMOUNT
               DISPLAY '  現在残高     : ' WS-ED-AMOUNT ' 円'
               MOVE POST-OUT-AVAILABLE TO WS-ED-AMOUNT
               DISPLAY '  お引出し可能 : ' WS-ED-AMOUNT ' 円'
               PERFORM WRITE-JRNL-END-OK
           ELSE
               PERFORM SHOW-ERROR
               PERFORM WRITE-JRNL-END-NG
           END-IF.
       TI-EXIT.
           EXIT.

      *================================================================
      * 出金 : 金種計画 → 記帳 → 繰出 (失敗時は記帳取消)
      *================================================================
       TXN-WITHDRAW SECTION.
       TW-START.
           PERFORM START-TRANSACTION
           SET SESS-TT-WITHDRAWAL TO TRUE
           PERFORM ACCEPT-AMOUNT
           IF WS-NUM-AMOUNT = ZERO
               GO TO TW-EXIT
           END-IF
           MOVE WS-NUM-AMOUNT TO SESS-TXN-AMOUNT
           PERFORM WRITE-JRNL-START

      *    -- (1) 紙幣を用意できるかを先に確認する
           SET CASH-FN-PLAN TO TRUE
           MOVE WS-NUM-AMOUNT  TO CASH-IN-AMOUNT
           CALL 'ATMCASH' USING CASH-PARM ATM-SESSION
           IF CASH-OUT-RETCODE NOT = RC-OK
               MOVE CASH-OUT-ERROR-CODE TO SESS-ERROR-CODE
               PERFORM SHOW-ERROR
               PERFORM WRITE-JRNL-END-NG
               GO TO TW-EXIT
           END-IF

      *    -- (2) 記帳
           SET POST-FN-WITHDRAW TO TRUE
           MOVE WS-NUM-AMOUNT    TO POST-IN-AMOUNT
           CALL 'ATMPOST' USING POST-PARM ATM-SESSION
           PERFORM COPY-POST-RESULT
           IF POST-OUT-RETCODE NOT = RC-OK
               PERFORM SHOW-ERROR
               PERFORM WRITE-JRNL-END-NG
               GO TO TW-EXIT
           END-IF

      *    -- (3) 繰出
           SET CASH-FN-DISPENSE TO TRUE
           CALL 'ATMCASH' USING CASH-PARM ATM-SESSION
           IF CASH-OUT-RETCODE NOT = RC-OK
               MOVE CASH-OUT-ERROR-CODE TO SESS-ERROR-CODE
               SET POST-DIR-CREDIT TO TRUE
               PERFORM REVERSE-POSTING
               PERFORM SHOW-ERROR
               GO TO TW-EXIT
           END-IF

           PERFORM SHOW-DISPENSE-DETAIL
           PERFORM SHOW-BALANCE-AFTER
           PERFORM WRITE-JRNL-END-OK.
       TW-EXIT.
           EXIT.

      *----------------------------------------------------------------
      * 記帳の巻き戻し。現金機構の失敗で取引が成立しなかったときに、
      * 記帳を戻して EJ に取消フェーズを残す。出金・入金の共通処理。
      * 戻す向きは呼出前に POST-IN-DIRECTION へ設定しておく。
      *----------------------------------------------------------------
       REVERSE-POSTING SECTION.
       RP-START.
           SET POST-FN-REVERSE TO TRUE
           MOVE SESS-TXN-AMOUNT TO POST-IN-AMOUNT
           CALL 'ATMPOST' USING POST-PARM ATM-SESSION

           SET JRNL-PH-REVERSAL TO TRUE
           IF POST-OUT-RETCODE = RC-OK
               SET JRNL-RS-SUCCESS TO TRUE
           ELSE
      *        -- 取消にも失敗。勘定不一致。係員対応が必要。
               SET JRNL-RS-FAILED TO TRUE
               DISPLAY '*** 障害が発生しました。'
                       '係員へお申し出ください。'
           END-IF
           PERFORM WRITE-JRNL
           PERFORM WRITE-JRNL-END-NG.
       RP-EXIT.
           EXIT.

      *================================================================
      * 入金 : 記帳 → 金庫収納 (収納失敗時は返却して記帳取消)
      *================================================================
       TXN-DEPOSIT SECTION.
       TD-START.
           PERFORM START-TRANSACTION
           SET SESS-TT-DEPOSIT    TO TRUE
           DISPLAY '紙幣を投入してください (金額を入力):'
           PERFORM ACCEPT-AMOUNT
           IF WS-NUM-AMOUNT = ZERO
               GO TO TD-EXIT
           END-IF
           MOVE WS-NUM-AMOUNT TO SESS-TXN-AMOUNT
           PERFORM WRITE-JRNL-START

           SET POST-FN-DEPOSIT TO TRUE
           MOVE WS-NUM-AMOUNT   TO POST-IN-AMOUNT
           CALL 'ATMPOST' USING POST-PARM ATM-SESSION
           PERFORM COPY-POST-RESULT
           IF POST-OUT-RETCODE NOT = RC-OK
               PERFORM SHOW-ERROR
               DISPLAY '紙幣をお返しします。'
               PERFORM WRITE-JRNL-END-NG
               GO TO TD-EXIT
           END-IF

           SET CASH-FN-ACCEPT TO TRUE
           MOVE WS-NUM-AMOUNT  TO CASH-IN-AMOUNT
           CALL 'ATMCASH' USING CASH-PARM ATM-SESSION
           IF CASH-OUT-RETCODE NOT = RC-OK
               MOVE CASH-OUT-ERROR-CODE TO SESS-ERROR-CODE
               DISPLAY '紙幣をお返しします。'
               SET POST-DIR-DEBIT TO TRUE
               PERFORM REVERSE-POSTING
               PERFORM SHOW-ERROR
               GO TO TD-EXIT
           END-IF

           PERFORM SHOW-BALANCE-AFTER
           PERFORM WRITE-JRNL-END-OK.
       TD-EXIT.
           EXIT.

      *================================================================
      * 振替
      *================================================================
       TXN-TRANSFER SECTION.
       TT-START.
           PERFORM START-TRANSACTION
           SET SESS-TT-TRANSFER   TO TRUE
           DISPLAY 'お振込先口座番号 (10 桁) を入力してください:'
           ACCEPT WS-IN-ACCT
           MOVE WS-IN-ACCT TO SESS-CPTY-ACCT-NO
           PERFORM ACCEPT-AMOUNT
           IF WS-NUM-AMOUNT = ZERO
               GO TO TT-EXIT
           END-IF
           MOVE WS-NUM-AMOUNT TO SESS-TXN-AMOUNT
           PERFORM WRITE-JRNL-START

           SET POST-FN-TRANSFER TO TRUE
           MOVE WS-NUM-AMOUNT    TO POST-IN-AMOUNT
           MOVE WS-IN-ACCT       TO POST-IN-CPTY-ACCT-NO
           CALL 'ATMPOST' USING POST-PARM ATM-SESSION
           PERFORM COPY-POST-RESULT

           EVALUATE POST-OUT-RETCODE
               WHEN RC-OK
                   PERFORM SHOW-BALANCE-AFTER
                   PERFORM WRITE-JRNL-END-OK
               WHEN RC-FATAL
                   DISPLAY '*** 取引が完了しませんでした。'
                           '係員へお申し出ください。'
                   PERFORM WRITE-JRNL-END-NG
               WHEN OTHER
                   PERFORM SHOW-ERROR
                   PERFORM WRITE-JRNL-END-NG
           END-EVALUATE.
       TT-EXIT.
           EXIT.

      *================================================================
      * 暗証番号変更
      *================================================================
       TXN-CHANGE-PIN SECTION.
       TC-START.
           PERFORM START-TRANSACTION
           SET SESS-TT-PIN-CHANGE TO TRUE
           MOVE ZERO TO SESS-TXN-AMOUNT
           PERFORM WRITE-JRNL-START

           DISPLAY '現在の暗証番号:'
           ACCEPT WS-IN-PIN
           DISPLAY '新しい暗証番号:'
           ACCEPT WS-IN-NEW-PIN

           SET AUTH-FN-CHANGE-PIN TO TRUE
           MOVE WS-IN-PIN          TO AUTH-IN-PIN
           MOVE WS-IN-NEW-PIN      TO AUTH-IN-NEW-PIN
           CALL 'ATMAUTH' USING AUTH-PARM ATM-SESSION

           IF AUTH-OUT-RETCODE = RC-OK
               DISPLAY '暗証番号を変更しました。'
               PERFORM WRITE-JRNL-END-OK
           ELSE
               MOVE AUTH-OUT-ERROR-CODE TO SESS-ERROR-CODE
               PERFORM SHOW-ERROR
               PERFORM WRITE-JRNL-END-NG
           END-IF.
       TC-EXIT.
           EXIT.

      *================================================================
      * 共通部品
      *================================================================
       START-TRANSACTION SECTION.
       ST-START.
           ADD 1 TO WS-TXN-SEQ
           PERFORM REFRESH-CLOCK
           MOVE SPACES TO SESS-TXN-ID
           STRING WS-CD-HHMMSS DELIMITED BY SIZE
                  WS-TXN-SEQ   DELIMITED BY SIZE
               INTO SESS-TXN-ID
           END-STRING
           MOVE ZERO   TO SESS-TXN-FEE
           MOVE ZERO   TO SESS-BAL-BEFORE SESS-BAL-AFTER
           MOVE EC-NONE TO SESS-ERROR-CODE
           MOVE SPACES TO SESS-CPTY-ACCT-NO
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > CN-CASSETTE-CNT
               MOVE ZERO TO SESS-DSP-CNT(WS-I)
               MOVE ZERO TO SESS-DSP-DENOM(WS-I)
           END-PERFORM.
       ST-EXIT.
           EXIT.

       ACCEPT-AMOUNT SECTION.
       AA-START.
           DISPLAY '金額を入力してください (円):'
           ACCEPT WS-IN-AMOUNT
           MOVE ZERO TO WS-NUM-AMOUNT
      *    -- TEST-NUMVAL は数値として解釈できれば 0 を返す
           IF FUNCTION TEST-NUMVAL (WS-IN-AMOUNT) = ZERO
               COMPUTE WS-NUM-AMOUNT = FUNCTION NUMVAL (WS-IN-AMOUNT)
           END-IF
           IF WS-NUM-AMOUNT = ZERO
               DISPLAY '金額が正しくありません。'
           END-IF.
       AA-EXIT.
           EXIT.

       COPY-POST-RESULT SECTION.
       CPR-START.
           MOVE POST-OUT-FEE        TO SESS-TXN-FEE
           MOVE POST-OUT-BAL-BEFORE TO SESS-BAL-BEFORE
           MOVE POST-OUT-BAL-AFTER  TO SESS-BAL-AFTER
           MOVE POST-OUT-ERROR-CODE TO SESS-ERROR-CODE.
       CPR-EXIT.
           EXIT.

       SHOW-BALANCE-AFTER SECTION.
       SBA-START.
           DISPLAY ' '
           IF SESS-TXN-FEE > ZERO
               MOVE SESS-TXN-FEE TO WS-ED-AMOUNT
               DISPLAY '  手数料   : ' WS-ED-AMOUNT ' 円'
           END-IF
           MOVE POST-OUT-BAL-AFTER TO WS-ED-AMOUNT
           DISPLAY '  取引後残高: ' WS-ED-AMOUNT ' 円'.
       SBA-EXIT.
           EXIT.

       SHOW-DISPENSE-DETAIL SECTION.
       SDD-START.
           DISPLAY ' '
           DISPLAY '  払出金種:'
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > CN-CASSETTE-CNT
               IF SESS-DSP-CNT(WS-I) > ZERO
                   MOVE SESS-DSP-DENOM(WS-I) TO WS-ED-DENOM
                   MOVE SESS-DSP-CNT(WS-I)   TO WS-ED-NOTES
                   DISPLAY '    ' WS-ED-DENOM ' 円券 x ' WS-ED-NOTES
                           ' 枚'
               END-IF
           END-PERFORM.
       SDD-EXIT.
           EXIT.

      *----------------------------------------------------------------
      * エラー表示。EJ にはコードを、画面には平易な文言を出す。
      *----------------------------------------------------------------
       SHOW-ERROR SECTION.
       SE-START.
           EVALUATE SESS-ERROR-CODE
               WHEN EC-CARD-UNKNOWN
                   MOVE 'お取扱いできないカードです' TO SESS-ERROR-MESSAGE
               WHEN EC-CARD-EXPIRED
                   MOVE 'カードの有効期限が切れています'
                       TO SESS-ERROR-MESSAGE
               WHEN EC-CARD-LOCKED
                   MOVE 'このカードは閉塞されています'
                       TO SESS-ERROR-MESSAGE
               WHEN EC-CARD-CAPTURED
                   MOVE 'このカードは無効です' TO SESS-ERROR-MESSAGE
               WHEN EC-PIN-INVALID
                   MOVE '暗証番号が違います' TO SESS-ERROR-MESSAGE
               WHEN EC-ACCT-UNKNOWN
                   MOVE '口座が見つかりません' TO SESS-ERROR-MESSAGE
               WHEN EC-ACCT-FROZEN
                   MOVE 'この口座はお取扱いを停止しています'
                       TO SESS-ERROR-MESSAGE
               WHEN EC-ACCT-CLOSED
                   MOVE 'この口座は解約済みです' TO SESS-ERROR-MESSAGE
               WHEN EC-ACCT-DORMANT
                   MOVE 'この口座は休眠状態です' TO SESS-ERROR-MESSAGE
               WHEN EC-INSUFFICIENT-FUNDS
                   MOVE '残高が不足しています' TO SESS-ERROR-MESSAGE
               WHEN EC-LIMIT-PER-TXN
                   MOVE '1 回のご利用限度額を超えています'
                       TO SESS-ERROR-MESSAGE
               WHEN EC-LIMIT-DAILY-AMT
                   MOVE '1 日のご利用限度額を超えています'
                       TO SESS-ERROR-MESSAGE
               WHEN EC-LIMIT-DAILY-CNT
                   MOVE '1 日のご利用回数を超えています'
                       TO SESS-ERROR-MESSAGE
               WHEN EC-AMOUNT-INVALID
                   MOVE 'ご指定の金額はお取扱いできません'
                       TO SESS-ERROR-MESSAGE
               WHEN EC-AMOUNT-NOT-UNIT
                   MOVE '1,000 円単位でご指定ください'
                       TO SESS-ERROR-MESSAGE
               WHEN EC-OUT-OF-SERVICE-HOUR
                   MOVE 'ただいまの時間はお取扱いできません'
                       TO SESS-ERROR-MESSAGE
               WHEN EC-CASH-SHORTAGE
                   MOVE 'ただいまこの金額はお取扱いできません'
                       TO SESS-ERROR-MESSAGE
               WHEN EC-CASH-NO-COMBINATION
                   MOVE '金種の都合によりお取扱いできません'
                       TO SESS-ERROR-MESSAGE
               WHEN EC-SYSTEM-BUSY
                   MOVE '混み合っています。少々お待ちください'
                       TO SESS-ERROR-MESSAGE
               WHEN EC-SYSTEM-IO
                   MOVE 'ただいまお取扱いできません'
                       TO SESS-ERROR-MESSAGE
               WHEN OTHER
                   MOVE 'お取扱いできません' TO SESS-ERROR-MESSAGE
           END-EVALUATE
           DISPLAY '  [' SESS-ERROR-CODE '] '
                   FUNCTION TRIM (SESS-ERROR-MESSAGE).
       SE-EXIT.
           EXIT.

      *----------------------------------------------------------------
      * EJ 出力。フェーズと結果区分は呼出前に SET しておく。
      *----------------------------------------------------------------
       WRITE-JRNL SECTION.
       WJ-START.
           PERFORM REFRESH-CLOCK
           SET JRNL-FN-WRITE TO TRUE
           CALL 'ATMJRNL' USING JRNL-PARM ATM-SESSION.
       WJ-EXIT.
           EXIT.

       WRITE-JRNL-START SECTION.
       WJS-START.
           SET JRNL-PH-START   TO TRUE
           SET JRNL-RS-SUCCESS TO TRUE
           PERFORM WRITE-JRNL.
       WJS-EXIT.
           EXIT.

       WRITE-JRNL-END-OK SECTION.
       WJO-START.
           MOVE EC-NONE TO SESS-ERROR-CODE
           SET JRNL-PH-END     TO TRUE
           SET JRNL-RS-SUCCESS TO TRUE
           PERFORM WRITE-JRNL.
       WJO-EXIT.
           EXIT.

       WRITE-JRNL-END-NG SECTION.
       WJN-START.
           SET JRNL-PH-END    TO TRUE
           SET JRNL-RS-FAILED TO TRUE
           PERFORM WRITE-JRNL.
       WJN-EXIT.
           EXIT.

       REFRESH-CLOCK SECTION.
       RC-START.
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE
           MOVE WS-CD-YYYYMMDD TO SESS-BUSINESS-DATE
           COMPUTE SESS-TIMESTAMP =
               WS-CD-YYYYMMDD * 1000000 + WS-CD-HHMMSS.
       RC-EXIT.
           EXIT.

       END PROGRAM ATMMAIN.
