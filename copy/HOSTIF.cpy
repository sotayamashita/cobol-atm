      *****************************************************************
      * HOSTIF.cpy - ATMHOST (勘定系ホスト模擬) 呼出インタフェース
      *   CALL 'ATMHOST' USING HOST-PARM ATM-SESSION
      *
      *   端末とホストのやりとりは電文単位で、レコード I/O ではない。
      *   1 電文が 1 つの完結した要求で、ホストはそれに 1 つの結果を
      *   返す。ATMACCT はこの電文を組み立てるだけの層になる。
      *
      *   [結果は 3 分類]
      *   成功・失敗の 2 値では足りない。応答が返らなかった場合、記帳
      *   されたかどうかは端末には判らない。これを失敗として扱うと、
      *   記帳済みの取引をもう一度記帳して二重記帳になる。成否不明を
      *   独立した分類として持ち、QUERY で解決する。
      *
      *   [冪等性]
      *   同じ HOST-IN-TXN-ID の POST は、二度目以降も一度目と同じ
      *   結果を返す。再送で二重記帳しないため。端末は応答が返らな
      *   ければ再送するしかなく、再送を安全にするのはホストの責務。
      *****************************************************************
       01  HOST-PARM.
           05  HOST-FUNCTION           PIC X(08).
      *        -- 口座照会。記帳の前提となる現在値を得る
               88  HOST-FN-INQUIRY             VALUE 'INQUIRY '.
      *        -- 記帳。更新後の口座レコードを渡して確定させる
               88  HOST-FN-POST                VALUE 'POST    '.
      *        -- 取引結果の照会。成否不明を解決する唯一の手段。
      *        -- 原取引が無ければ「記帳されていない」と判る。
               88  HOST-FN-QUERY               VALUE 'QUERY   '.
               88  HOST-FN-CLOSE               VALUE 'CLOSE   '.
      *    -- 冪等キー。端末が取引ごとに採番した ID をそのまま使う。
      *    -- 再送では同じ値を送ること。採番し直すとホストが別取引と
      *    -- みなし、二重記帳になる。
           05  HOST-IN-TXN-ID          PIC X(12).
           05  HOST-IN-ACCT-NO         PIC X(10).
      *    -- 端末が照会時に受け取った版数。ホストはこれと現物を比べて
      *    -- 排他する。ローカル元帳のときの楽観ロックがここへ移る。
           05  HOST-IN-VERSION         PIC 9(09).
           05  HOST-IO-RECORD          PIC X(160).
           05  HOST-OUT-RETCODE        PIC S9(04) COMP.
           05  HOST-OUT-ERROR-CODE     PIC X(04).
      *    -- 端末が分岐する唯一の軸。RETCODE は技術的な成否で、
      *    -- こちらは業務としてどう扱うかを表す。
           05  HOST-OUT-OUTCOME        PIC X(01).
               88  HOST-OC-SUCCESS             VALUE 'S'.
               88  HOST-OC-FAILURE             VALUE 'F'.
               88  HOST-OC-UNKNOWN             VALUE 'U'.
      *    -- QUERY の結果。原取引が見つかったか。
           05  HOST-OUT-FOUND          PIC X(01).
               88  HOST-FOUND-YES              VALUE 'Y'.
