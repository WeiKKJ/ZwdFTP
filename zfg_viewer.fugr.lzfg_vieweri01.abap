*----------------------------------------------------------------------*
***INCLUDE LZFG_VIEWERI01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN '&F03' OR 'EXIT' OR 'CANC' OR 'BACK' OR 'CANCEL' OR '&F12' OR '&F15'.
      IF NOT ( lo_docking_container IS INITIAL ).
        CALL METHOD lo_docking_container->free
          EXCEPTIONS
            cntl_error        = 1
            cntl_system_error = 2
            OTHERS            = 3.
      ENDIF.

      IF NOT ( lo_html IS INITIAL ).
        CALL METHOD lo_html->free
          EXCEPTIONS
            cntl_error        = 1
            cntl_system_error = 2
            OTHERS            = 3.
      ENDIF.

      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.
