      *****************************************************************
      * PROGRAM : ATMPURGE
      * PURPOSE : 退避済み電子ジャーナルの保存年限管理
      * DESIGN  :
      *   締めのたびに当日分の EJ が data/atmjrnl-YYYYMMDD.dat へ退避
      *   される。退避済みファイルは自動では消えない。保存年限は監査
      *   要件であり、何日残すかは運用が決めることだからである。
      *
      *   このバッチはその方針を「実行するための道具」であって、方針を
      *   決める場所ではない。したがって保存年限に既定値を持たせない。
      *   入力が無ければ何もせずに終わる。既定値を置くと、方針を決めない
      *   まま実行されて監査対象が消える。
      *
      *   [消す前に示す]
      *   削除は元に戻せない。まず対象を集めて件数と日付の範囲を示し、
      *   係員の確認を受けてから消す。確認が得られなければ何もしない。
      *   ATMJRNL は集めた対象を保持し、削除ではそれだけを消す。走査を
      *   やり直さないので、係員が見たものと消えるものが必ず一致する。
      *
      *   [走査範囲]
      *   ディレクトリを列挙する標準的な手段が無いため、対象日から遡って
      *   日付を総当たりする。無制限に遡ると起点が定まらないので、
      *   走査日数はこのバッチが決めて ATMJRNL へ渡す。
      *
      *   [責務]
      *   EJ のファイルを持つのは ATMJRNL だけという規約に従い、
      *   ファイル名の組み立てと削除は ATMJRNL が行う。このバッチが
      *   持つのは保存年限の受け取りと、対象日の算出だけである。
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ATMPURGE.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-CONST.
      *    -- 遡る日数の上限。10 年分あれば実用上足りる。無制限に
      *    -- しないのは、総当たりの起点を定める必要があるため。
           05  WS-SCAN-DAYS            PIC 9(05) VALUE 03660.

       01  WS-INPUT.
           05  WS-IN-LINE              PIC X(12) VALUE SPACES.
           05  WS-NUM-SIGNED           PIC S9(10) VALUE ZERO.
           05  WS-RETAIN-DAYS          PIC 9(05) VALUE ZERO.

       01  WS-WORK.
           05  WS-CUTOFF               PIC 9(08) VALUE ZERO.
           05  WS-ABORT                PIC X(01) VALUE 'N'.

       01  WS-EDIT.
           05  WS-ED-CNT               PIC ZZZ,ZZ9.
           05  WS-ED-DAYS              PIC ZZ,ZZ9.

       COPY 'ATMCONST.cpy'.
       COPY 'RETCODE.cpy'.
       COPY 'ATMSESS.cpy'.
       COPY 'JRNLIF.cpy'.

       01  WS-DATETIME.
           05  WS-CURRENT-DATE.
               10  WS-CD-YYYYMMDD      PIC 9(08).
               10  WS-CD-HHMMSS        PIC 9(06).
               10  FILLER              PIC X(15).

       PROCEDURE DIVISION.

       MAIN-CONTROL SECTION.
       MAIN-START.
           PERFORM INITIALIZE-BATCH
           PERFORM ACCEPT-RETENTION
           IF WS-ABORT = 'Y'
               STOP RUN
           END-IF

           PERFORM RESOLVE-CUTOFF
           PERFORM LIST-TARGETS
           IF WS-ABORT = 'Y'
               STOP RUN
           END-IF

           PERFORM CONFIRM-AND-PURGE
           STOP RUN.

       INITIALIZE-BATCH SECTION.
       INIT-START.
           INITIALIZE ATM-SESSION
           MOVE CN-ATM-ID TO SESS-ATM-ID
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE
           MOVE WS-CD-YYYYMMDD TO SESS-BUSINESS-DATE

           DISPLAY ' '
           DISPLAY '=== 退避 EJ の保存年限管理 端末 ' CN-ATM-ID
                   ' 基準日 ' SESS-BUSINESS-DATE ' ==='.
       INIT-EXIT.
           EXIT.

      *================================================================
      * 保存年限の受け取り。既定値は持たない。
      *   何日残すかは監査要件であり、このバッチが決めることではない。
      *================================================================
       ACCEPT-RETENTION SECTION.
       AR-START.
           DISPLAY ' '
           DISPLAY '  保存年限を日数で入力してください'
                   ' (空のまま Enter で中止します):'
      *    -- 入力前に必ず消す。入力が尽きた場合 ACCEPT は項目を
      *    -- 変えないため、前の入力が残る。
           MOVE SPACES TO WS-IN-LINE
           ACCEPT WS-IN-LINE

           IF WS-IN-LINE = SPACES
               DISPLAY '  保存年限が指定されていません。'
                       '何も削除していません。'
               MOVE 'Y' TO WS-ABORT
               GO TO AR-EXIT
           END-IF

      *    -- TEST-NUMVAL は数値として解釈できれば 0 を返す
           IF FUNCTION TEST-NUMVAL (WS-IN-LINE) NOT = ZERO
               DISPLAY '  日数が正しくありません。何も削除していません。'
               MOVE 'Y' TO WS-ABORT
               GO TO AR-EXIT
           END-IF

      *    -- 符号付きで受けてから判定する。符号なし項目へ直接受けると、
      *    -- 負数が絶対値に化けて素通りする。
           COMPUTE WS-NUM-SIGNED = FUNCTION NUMVAL (WS-IN-LINE)
           IF WS-NUM-SIGNED < 1 OR WS-NUM-SIGNED > WS-SCAN-DAYS
               MOVE WS-SCAN-DAYS TO WS-ED-DAYS
               DISPLAY '  日数は 1 から' WS-ED-DAYS
                       ' の範囲で指定してください。'
                       '何も削除していません。'
               MOVE 'Y' TO WS-ABORT
               GO TO AR-EXIT
           END-IF

           MOVE WS-NUM-SIGNED TO WS-RETAIN-DAYS.
       AR-EXIT.
           EXIT.

      *================================================================
      * 対象日の算出。基準日から保存年限だけ遡った日より前が対象。
      *   その日自身は「保存年限の最終日」なので残す。
      *================================================================
       RESOLVE-CUTOFF SECTION.
       RC-START.
           COMPUTE WS-CUTOFF = FUNCTION DATE-OF-INTEGER (
               FUNCTION INTEGER-OF-DATE (SESS-BUSINESS-DATE)
               - WS-RETAIN-DAYS)

           MOVE WS-RETAIN-DAYS TO WS-ED-DAYS
           DISPLAY ' '
           DISPLAY '  保存年限: ' WS-ED-DAYS ' 日'
           DISPLAY '  ' WS-CUTOFF ' より前の退避 EJ が対象です。'.
       RC-EXIT.
           EXIT.

       LIST-TARGETS SECTION.
       LT-START.
           SET JRNL-PG-LIST TO TRUE
           PERFORM CALL-PURGE
           IF JRNL-OUT-RETCODE NOT = RC-OK
               DISPLAY '  退避 EJ を確認できません。'
               MOVE 'Y' TO WS-ABORT
               GO TO LT-EXIT
           END-IF

           IF JRNL-OUT-PURGED-CNT = ZERO
               DISPLAY '  対象の退避 EJ はありません。'
                       '何も削除していません。'
               MOVE 'Y' TO WS-ABORT
               GO TO LT-EXIT
           END-IF

           MOVE JRNL-OUT-PURGED-CNT TO WS-ED-CNT
           DISPLAY '  対象: ' WS-ED-CNT ' 件 ('
                   JRNL-OUT-PURGE-OLDEST ' 〜 '
                   JRNL-OUT-PURGE-NEWEST ')'.
       LT-EXIT.
           EXIT.

      *================================================================
      * 確認してから消す。削除は元に戻せないので、明示の同意を要する。
      *   'Y' 以外はすべて中止として扱う。既定を削除側に倒さない。
      *================================================================
       CONFIRM-AND-PURGE SECTION.
       CAP-START.
           DISPLAY ' '
           DISPLAY '  削除しますか。監査の保存年限を確認してください。'
                   ' (Y で削除):'
           MOVE SPACES TO WS-IN-LINE
           ACCEPT WS-IN-LINE

           IF WS-IN-LINE NOT = 'Y'
               DISPLAY '  中止しました。何も削除していません。'
               GO TO CAP-EXIT
           END-IF

           SET JRNL-PG-DELETE TO TRUE
           PERFORM CALL-PURGE

           MOVE JRNL-OUT-PURGED-CNT TO WS-ED-CNT
           IF JRNL-OUT-RETCODE = RC-OK
               DISPLAY '  削除しました: ' WS-ED-CNT ' 件'
           ELSE
               DISPLAY '  [' EC-SYSTEM-IO '] 削除の途中で失敗しました。'
                       '削除できたのは ' WS-ED-CNT ' 件です。'
           END-IF.
       CAP-EXIT.
           EXIT.

      *----------------------------------------------------------------
      * PURGE の呼出。モードは呼出前に設定しておく。
      *   パラメタを組む手順と呼出を 1 箇所にまとめておかないと、
      *   JRNLIF に項目が増えたときに片方だけ直して気づけない。
      *----------------------------------------------------------------
       CALL-PURGE SECTION.
       CP-START.
           SET JRNL-FN-PURGE TO TRUE
           MOVE WS-CUTOFF    TO JRNL-IN-PURGE-BEFORE
           MOVE WS-SCAN-DAYS TO JRNL-IN-PURGE-DAYS
           CALL 'ATMJRNL' USING JRNL-PARM ATM-SESSION.
       CP-EXIT.
           EXIT.

       END PROGRAM ATMPURGE.
