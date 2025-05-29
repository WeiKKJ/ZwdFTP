FUNCTION-POOL zfg_viewer.                   "MESSAGE-ID ..

* INCLUDE LZFG_VIEWERD...                    " Local class definition
DATA:gt_data              TYPE TABLE OF x255,
     lo_docking_container TYPE REF TO cl_gui_docking_container,
     lo_html              TYPE REF TO cl_gui_html_viewer,
     lv_url               TYPE char255,
     lv_len               TYPE i,
     ok_code(20)          TYPE c,
     lo_pdf               TYPE REF TO zcl_pdf,
     dtype                TYPE c,
     dsubtype             TYPE c.
