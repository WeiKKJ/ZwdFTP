*----------------------------------------------------------------------*
***INCLUDE LZFG_VIEWERO01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'STANDARD'.
  CLEAR ok_code.
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

  CREATE OBJECT lo_docking_container
    EXPORTING
      repid     = sy-repid
      dynnr     = sy-dynnr
      extension = 5000.

  CREATE OBJECT lo_html
    EXPORTING
      parent = lo_docking_container.
*
* Load the HTML
  lo_html->load_data(
    EXPORTING
      type         = dtype
      subtype      = dsubtype
*      subtype      = `xlsx`
    IMPORTING
      assigned_url         = lv_url
    CHANGING
      data_table           = gt_data
    EXCEPTIONS
      dp_invalid_parameter = 1
      dp_error_general     = 2
      cntl_error           = 3
      OTHERS               = 4 ).

* Show it

  lo_html->show_url( url = lv_url  in_place = 'X' ).
ENDMODULE.
