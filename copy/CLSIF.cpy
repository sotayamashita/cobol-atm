      *****************************************************************
      * CLSIF.cpy - ATMCLS (端末状態) 呼出インタフェース
      *   CALL 'ATMCLS' USING CLS-PARM ATM-SESSION
      *
      *   端末単位で永続する状態を所有する。他のマスタと同じく、
      *   ファイルを持つのは専用モジュール 1 つという規約に従う。
      *
      *   持つものは 2 つ。
      *   (1) 締め状態  : 最後に締めた営業日と実行中フラグ
      *   (2) 採番      : 端末内で一意な連番の発行
      *
      *   採番をここに置くのは、必要なのが「再起動をまたいで単調増加
      *   する識別子の発行者」だからである。EJ の通番を覗いて代用する
      *   と、番号を 1 つ得るだけで EJ の追記経路が開いてしまい、
      *   走査中の呼出で通番の意味も曖昧になる。永続状態を持つ
      *   モジュールが発行するのが素直。
      *
      *   採番は取引 ID とセッション ID の両方に使う。用途の区別は
      *   呼出元が前置する記号 ('T' / 'SES') で表す。
      *****************************************************************
       01  CLS-PARM.
           05  CLS-FUNCTION            PIC X(08).
      *        -- 締めてよいかを判定する。状態は変えない
               88  CLS-FN-CHECK                VALUE 'CHECK   '.
      *        -- 締めの開始を記録する (実行中フラグを立てる)
               88  CLS-FN-START                VALUE 'START   '.
      *        -- 締めの完了を記録する
               88  CLS-FN-FINISH               VALUE 'FINISH  '.
      *        -- 実行中フラグの解除。係員が原因を確認したあとに使う。
      *        -- 自動解除は多重実行を許すので設けない。
               88  CLS-FN-RELEASE              VALUE 'RELEASE '.
      *        -- 端末内で一意な連番を 1 つ払い出す
               88  CLS-FN-NEXT-NO              VALUE 'NEXTNO  '.
               88  CLS-FN-CLOSE                VALUE 'CLOSE   '.
           05  CLS-IN-DIFF-CNT         PIC 9(05).
           05  CLS-IN-PENDING-CNT      PIC 9(05).
           05  CLS-OUT-RETCODE         PIC S9(04) COMP.
           05  CLS-OUT-ERROR-CODE      PIC X(04).
           05  CLS-OUT-NEXT-NO         PIC 9(09).
           05  CLS-OUT-LAST-CLOSED     PIC 9(08).
