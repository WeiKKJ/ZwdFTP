FUNCTION zfm_cl_gui_html_viewer.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(DATAX) TYPE  XSTRING
*"     VALUE(DTYPE) TYPE  C DEFAULT 'text'
*"     VALUE(DSUBTYPE) TYPE  C DEFAULT 'html'
*"----------------------------------------------------------------------
  CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
    EXPORTING
      buffer        = datax
    IMPORTING
      output_length = lv_len
    TABLES
      binary_tab    = gt_data.
  dtype = dtype.
  dsubtype = dsubtype.
  IF NOT gt_data IS INITIAL.
    CALL SCREEN 100.
  ENDIF.


ENDFUNCTION.
