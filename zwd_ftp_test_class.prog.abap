*&---------------------------------------------------------------------*
*& 包含               ZWD_FTP_TEST_CLASS
*&---------------------------------------------------------------------*
*======================================================================
CLASS lcl_ftp DEFINITION.

  " ======================================================================
  PUBLIC SECTION.

    METHODS connect IMPORTING iv_hostname TYPE zcl_wd_ftp=>mty_hostname
                              iv_port     TYPE i
                              iv_username TYPE zcl_wd_ftp=>mty_username
                              iv_password TYPE zcl_wd_ftp=>mty_password.

    METHODS disconnect.

    " ======================================================================
  PRIVATE SECTION.

    CONSTANTS:
      mc_func_upload TYPE salv_de_function VALUE '%%_UPLOAD_%%'.

    DATA mo_ftp     TYPE REF TO zcl_wd_ftp.
    DATA mo_salv    TYPE REF TO cl_salv_table.
    DATA mt_listing TYPE zcl_wd_ftp=>mty_directory_listing_tt.
    METHODS set_header.
    METHODS refresh_listing.
    METHODS display.
    METHODS handle_double_click FOR EVENT if_salv_events_actions_table~double_click OF cl_salv_events_table IMPORTING row column.
    METHODS handle_before_salv_function FOR EVENT if_salv_events_functions~before_salv_function OF cl_salv_events_table IMPORTING e_salv_function.
    METHODS change_dir IMPORTING iv_dir TYPE zcl_wd_ftp=>mty_filename.
    METHODS download IMPORTING iv_fname TYPE zcl_wd_ftp=>mty_filename.
    " ======================================================================
ENDCLASS.


CLASS lcl_ftp IMPLEMENTATION.
  " ======================================================================
  METHOD connect.
    "  ---------------------------------------------------------------------
    TRY.
        mo_ftp = NEW #( iv_hostname = iv_hostname
                        iv_port     = iv_port
                        iv_username = iv_username
                        iv_password = iv_password ).

        refresh_listing( ).

      CATCH zcx_wd_ftp_error INTO DATA(gx_error).
        MESSAGE gx_error->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.
    "  ---------------------------------------------------------------------
    TRY.
        cl_salv_table=>factory( "EXPORTING r_container  = cl_gui_container=>default_screen
                                IMPORTING r_salv_table = mo_salv
                                CHANGING  t_table      = mt_listing ).
        mo_salv->get_columns( )->get_column( 'TYPE' )->set_output_length( 4 ).
        mo_salv->get_columns( )->get_column( 'TYPE' )->set_long_text( '类型' ).
        mo_salv->get_columns( )->get_column( 'FILESIZE' )->set_output_length( 14 ).
        mo_salv->get_columns( )->get_column( 'FILESIZE' )->set_long_text( '大小' ).
        mo_salv->get_columns( )->get_column( 'FILEDATE' )->set_output_length( 12 ).
        mo_salv->get_columns( )->get_column( 'FILEDATE' )->set_long_text( '修改日期' ).
        mo_salv->get_columns( )->get_column( 'FILENAME' )->set_output_length( 200 ).
        mo_salv->get_columns( )->get_column( 'FILENAME' )->set_long_text( '名称' ).
        DATA(functions) = mo_salv->get_functions( ).
        functions->set_default( ).
*        functions->set_all( abap_true ).
*        TRY.
*            functions->add_function(
*              name     = 'REMOVE'
*              icon     = CONV string( icon_delete )
*              text     = 'Remove'
*              tooltip  = 'Remove selected weak password hashes'
*              position = if_salv_c_function_position=>right_of_salv_functions ).
*          CATCH cx_salv_existing cx_salv_wrong_call.
*        ENDTRY.

        functions->set_export_localfile( ).

        mo_salv->get_display_settings( )->set_striped_pattern( abap_true ).

        SET HANDLER handle_double_click         FOR mo_salv->get_event( ).
        SET HANDLER handle_before_salv_function FOR mo_salv->get_event( ).
        DATA(lr_display_settings) = mo_salv->get_display_settings( ).
        lr_display_settings->set_list_header_size( cl_salv_display_settings=>c_header_size_small ).
        lr_display_settings->set_no_merging( if_salv_c_bool_sap=>true ).
        display( ).

      CATCH cx_salv_msg cx_salv_not_found.
        BREAK-POINT.
    ENDTRY.

    "  ---------------------------------------------------------------------
  ENDMETHOD.

  " ======================================================================
  METHOD disconnect.
    "  ---------------------------------------------------------------------
    CHECK mo_ftp IS BOUND.
    mo_ftp->disconnect( ).

    "  ---------------------------------------------------------------------
  ENDMETHOD.

  " ======================================================================
  METHOD set_header.
    "  ---------------------------------------------------------------------
    DATA:
      lv_header TYPE lvc_title.

    "  ---------------------------------------------------------------------
    lv_header = mo_ftp->get_current_directory( ).

    "  ---------------------------------------------------------------------
    mo_salv->get_display_settings( )->set_list_header( lv_header ).

    "  ---------------------------------------------------------------------
  ENDMETHOD.

  " ======================================================================
  METHOD refresh_listing.
    "  ---------------------------------------------------------------------
    CLEAR mt_listing.
    mt_listing = mo_ftp->get_directory_listing( ).
    INSERT VALUE #( type     = zcl_wd_ftp=>mc_type_dir
                    filename = '..'                    ) INTO mt_listing INDEX 1.

    "  ---------------------------------------------------------------------
  ENDMETHOD.

  " ======================================================================
  METHOD display.
    "  ---------------------------------------------------------------------
    set_header( ).

    "  ---------------------------------------------------------------------
    mo_salv->display( ).

    "  ---------------------------------------------------------------------
  ENDMETHOD.

  " ======================================================================
  METHOD handle_double_click.
    "  ---------------------------------------------------------------------
    FIELD-SYMBOLS:
      <ls_line> LIKE LINE OF mt_listing.

    "  ---------------------------------------------------------------------
    UNASSIGN <ls_line>.
    ASSIGN mt_listing[ row ] TO <ls_line>.
    IF  <ls_line> IS ASSIGNED.
      CASE <ls_line>-type.
        WHEN zcl_wd_ftp=>mc_type_dir
        OR   zcl_wd_ftp=>mc_type_link.
          change_dir( <ls_line>-filename ).
        WHEN zcl_wd_ftp=>mc_type_file.
          download( <ls_line>-filename ).
        WHEN OTHERS.
      ENDCASE.
    ENDIF.

    "  ---------------------------------------------------------------------
  ENDMETHOD.

  " ======================================================================
  METHOD handle_before_salv_function.
    "  ---------------------------------------------------------------------
    DATA lt_file_table  TYPE filetable.
    DATA lv_rc          TYPE i.
    DATA lv_action      TYPE i.
    DATA lv_blob_length TYPE i.
    DATA lt_blob_data   TYPE zcl_wd_ftp=>mty_blob_tt.
    DATA lv_filename    TYPE zcl_wd_ftp=>mty_filename.
    DATA lv_dummy       TYPE string.

    "  ---------------------------------------------------------------------
    " only hijack "local file" button, ignore all others
    IF e_salv_function <> '%PC'.
      RETURN.
    ENDIF.

    "  ---------------------------------------------------------------------
    FREE: lt_file_table, lv_rc, lv_action.
    cl_gui_frontend_services=>file_open_dialog( EXPORTING  multiselection = abap_true
                                                CHANGING   file_table     = lt_file_table
                                                           rc             = lv_rc
                                                           user_action    = lv_action
                                                EXCEPTIONS OTHERS         = 4             ).
    IF sy-subrc <> 0
    OR lv_rc    <  0
    OR lv_action <> cl_gui_frontend_services=>action_ok
    OR lt_file_table IS INITIAL.
      MESSAGE 'Canceled' TYPE 'E' DISPLAY LIKE 'W'.
    ENDIF.

    "  ---------------------------------------------------------------------
    LOOP AT lt_file_table ASSIGNING FIELD-SYMBOL(<ls_file>).
      FREE: lv_blob_length, lt_blob_data, lv_filename.
      cl_gui_frontend_services=>gui_upload( EXPORTING  filename   = |{ <ls_file>-filename }|
                                                       filetype   = 'BIN'
                                            IMPORTING  filelength = lv_blob_length
                                            CHANGING   data_tab   = lt_blob_data
                                            EXCEPTIONS OTHERS     = 4                        ).
      IF sy-subrc = 0.
        lv_filename = <ls_file>-filename.
        lv_filename = reverse( lv_filename ).
        SPLIT lv_filename AT '\' INTO lv_filename lv_dummy.
        lv_filename = reverse( lv_filename ).
        mo_ftp->upload_table( iv_filename    = lv_filename
                              it_bin_data    = lt_blob_data
                              iv_blob_length = lv_blob_length ).
      ENDIF.
    ENDLOOP.

    "  ---------------------------------------------------------------------
    refresh_listing( ).
    mo_salv->refresh( refresh_mode = if_salv_c_refresh=>full ).

    "  ---------------------------------------------------------------------
    " this forces the default function to be canceled
    MESSAGE '' TYPE 'E'.

    "  ---------------------------------------------------------------------
  ENDMETHOD.

  " ======================================================================
  METHOD change_dir.
    "  ---------------------------------------------------------------------
    DATA:
      lv_dir TYPE zcl_wd_ftp=>mty_directory.

    "  ---------------------------------------------------------------------
    lv_dir = iv_dir.
    mo_ftp->change_directory( lv_dir ).

    "  ---------------------------------------------------------------------
    refresh_listing( ).

    "  ---------------------------------------------------------------------
    mo_salv->refresh( refresh_mode = if_salv_c_refresh=>full ).

    "  ---------------------------------------------------------------------
    display( ).

    "  ---------------------------------------------------------------------
  ENDMETHOD.

  " ======================================================================
  METHOD download.
    "  ---------------------------------------------------------------------
    DATA lv_default_file_name TYPE string.
    DATA lv_file_filter       TYPE string.
    DATA lv_ext               TYPE string.
    DATA lv_initial_directory TYPE string.
    DATA lv_filename          TYPE string.
    DATA lv_path              TYPE string.
    DATA lv_fullpath          TYPE string.
    DATA lv_action            TYPE i.
    DATA lt_bin_data          TYPE zcl_wd_ftp=>mty_blob_tt.
    DATA lv_blob_length       TYPE i.

*    CALL METHOD mo_ftp->download_xstring
*      EXPORTING
*        iv_filename = iv_fname
*      RECEIVING
*        rv_bin_data = DATA(datax).
*
*    CALL FUNCTION 'ZFM_CL_GUI_HTML_VIEWER'
*      EXPORTING
*        datax = datax
**       DTYPE = 'text'
**       DSUBTYPE       = 'html'
*      .
*    RETURN.
    DATA(opsys) = sy-opsys.
    sy-opsys = 'Windows NT'.
    DATA(pure_extension) = cl_fs_windows_path=>create( name = CONV string( iv_fname ) )->get_file_extension( ).
    sy-opsys = opsys.
    CASE pure_extension.
      WHEN '.pdf'.
        DATA(datax) = mo_ftp->download_xstring( iv_filename = iv_fname ).

        CALL FUNCTION 'ZFM_CL_GUI_HTML_VIEWER'
          EXPORTING datax = datax.
        " DTYPE = 'text'
        " DSUBTYPE       = 'html'

      WHEN OTHERS.
        "  ---------------------------------------------------------------------
        lv_default_file_name = iv_fname.

        "  ---------------------------------------------------------------------
        " get file extension
        lv_file_filter = reverse( iv_fname ).
        SPLIT lv_file_filter AT '.' INTO lv_ext lv_file_filter.
        IF lv_file_filter IS NOT INITIAL.
          lv_ext = reverse( lv_ext ).
        ELSE.
          FREE lv_ext.
        ENDIF.

        "  ---------------------------------------------------------------------
        " build filter string like (*.TXT)|*.TXT|
        IF lv_ext IS NOT INITIAL.
          FREE lv_file_filter.
          lv_file_filter = |(*.{ lv_ext })\|*.{ lv_ext }\||.
        ENDIF.

        "  ---------------------------------------------------------------------
        cl_gui_frontend_services=>file_save_dialog( EXPORTING  default_file_name = lv_default_file_name
                                                               file_filter       = lv_file_filter
                                                    CHANGING   filename          = lv_filename
                                                               path              = lv_path
                                                               fullpath          = lv_fullpath
                                                               user_action       = lv_action
                                                    EXCEPTIONS OTHERS            = 4                    ).

        IF sy-subrc <> 0
        OR lv_action = cl_gui_frontend_services=>action_cancel
        OR lv_fullpath IS INITIAL.
          MESSAGE 'Canceled' TYPE 'S' DISPLAY LIKE 'W'.
          RETURN.
        ENDIF.

        "  ---------------------------------------------------------------------
        mo_ftp->download_table( EXPORTING iv_filename    = iv_fname
                                IMPORTING et_bin_data    = lt_bin_data
                                          ev_blob_length = lv_blob_length ).

        "  ---------------------------------------------------------------------
        cl_gui_frontend_services=>gui_download( EXPORTING  bin_filesize = lv_blob_length
                                                           filename     = lv_fullpath
                                                           filetype     = 'BIN'
                                                CHANGING   data_tab     = lt_bin_data
                                                EXCEPTIONS OTHERS       = 4               ).
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                     WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.
    ENDCASE.
    "  ---------------------------------------------------------------------
  ENDMETHOD.
ENDCLASS.
