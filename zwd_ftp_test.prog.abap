*&---------------------------------------------------------------------*
*& Report  ZWD_FTP_TEST
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zwd_ftp_test.
INCLUDE zwd_ftp_test_class.


*======================================================================
DATA:
  go_ftp TYPE REF TO lcl_ftp.

PARAMETERS:
  hostname TYPE zcl_wd_ftp=>mty_hostname LOWER CASE MEMORY ID hostname,
  port     TYPE i DEFAULT 21,
  username TYPE zcl_wd_ftp=>mty_username LOWER CASE MEMORY ID username,
  password TYPE zcl_wd_ftp=>mty_password LOWER CASE MEMORY ID password MODIF ID sec.

*======================================================================
INITIALIZATION.

* ---------------------------------------------------------------------
  go_ftp = NEW #( ).

*======================================================================
AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF screen-group1 = 'SEC' AND screen-group3 EQ 'PAR'.
      screen-invisible = 1. "输入时显示为 *（星号）
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

AT SELECTION-SCREEN.
  IF hostname IS INITIAL.
    GET PARAMETER ID 'ZFTP_HOST' FIELD hostname.
  ENDIF.
  IF username IS INITIAL.
    GET PARAMETER ID 'ZFTP_UNAME' FIELD username.
  ENDIF.
  IF password IS INITIAL.
    GET PARAMETER ID 'ZFTP_PWD' FIELD password.
  ENDIF.
*======================================================================
START-OF-SELECTION.
*  WRITE: space.
* ---------------------------------------------------------------------
  go_ftp->connect( iv_hostname = hostname
                   iv_port     = port
                   iv_username = username
                   iv_password = password ).

* ---------------------------------------------------------------------
  go_ftp->disconnect( ).
