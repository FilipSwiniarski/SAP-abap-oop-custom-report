*&---------------------------------------------------------------------*
*& Report  ZDTC_FS_REPORT_CL_GUI_ALV2
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zdtc_fs_report_cl_gui_alv2. "program name
*&---------------------------------------------------------------------*
CLASS lcl_for_grid_event DEFINITION. "class for handling events definition
  PUBLIC SECTION.
    METHODS:
      handle_double_click
      FOR EVENT double_click
                  OF cl_gui_alv_grid
        IMPORTING e_row e_column es_row_no,

      handle_hotspot_click
      FOR EVENT hotspot_click
                  OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id es_row_no sender,
      handle_close
                  FOR EVENT close OF cl_gui_dialogbox_container
        IMPORTING sender.
  PRIVATE SECTION.
    DATA: dialogbox_status TYPE c.
ENDCLASS.
*&---------------------------------------------------------------------*
TYPES: BEGIN OF ty_vbak_vbap, "structure for internal table
         vbeln  TYPE vbeln, "vbak
         kunnr  TYPE kunnr, "vbak
         matnr  TYPE matnr, "vbap
         arktx  TYPE arktx, "vbap
         kwmeng TYPE kwmeng, "vbap quantity
         netwr  TYPE netwr, "vbak
         meins  TYPE meins, "vbap unit of messure
         werks  TYPE vbap-werks, "vbap
         vkorg  TYPE vkorg, "vbak
         erdat  TYPE erdat, "vbak
       END OF ty_vbak_vbap.
DATA: gt_vbak_vbap        TYPE TABLE OF ty_vbak_vbap, "internal table type of ty_vbak_vbap
      gt_vbap             TYPE TABLE OF vbap, "internal table type of vbap
      wa_vbak_vbap        TYPE ty_vbak_vbap, "working area to process data
      grid2               TYPE REF TO cl_gui_alv_grid, "declaration of grid2 object type of cl_gui_alv_grid class
      dialogbox_container TYPE REF TO cl_gui_dialogbox_container. "declaration of dialogbox object type of cl_gui_alv_grid class
*&---------------------------------------------------------------------
CLASS lcl_for_grid_event IMPLEMENTATION.

  METHOD handle_double_click. "method to handle double click on alv which opens POP_UP window
    DATA: ls_vbap LIKE LINE OF gt_vbak_vbap.
    READ TABLE gt_vbak_vbap INDEX e_row-index INTO ls_vbap.
    PERFORM select_table_vbap USING ls_vbap
                              CHANGING gt_vbap.
    IF dialogbox_status IS INITIAL. "check if POP_UP window exists
      dialogbox_status = 'X'.
      PERFORM create_detail_list.
    ELSE.
      CALL METHOD dialogbox_container->set_visible
        EXPORTING
          visible = 'X'.
      CALL METHOD grid2->refresh_table_display.
    ENDIF.
  ENDMETHOD.

  METHOD handle_close. "method to handle closing of POP_UP window
    CALL METHOD sender->set_visible
      EXPORTING
        visible = space.
  ENDMETHOD.

  METHOD handle_hotspot_click. "method to handle hotspot click on alv columns (vbeln, matnr), it forwards to transactions VA03 or M03
    CASE e_column_id.
      WHEN 'VBELN'.
        READ TABLE gt_vbak_vbap INDEX e_row_id INTO wa_vbak_vbap.
        IF sy-subrc = 0.
          SET PARAMETER ID 'AUN' FIELD wa_vbak_vbap-vbeln.
          CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.
        ENDIF.
      WHEN 'MATNR'.
        READ TABLE gt_vbak_vbap INDEX e_row_id INTO wa_vbak_vbap.
        IF sy-subrc = 0.
          SET PARAMETER ID 'MAT' FIELD wa_vbak_vbap-matnr.
          CALL TRANSACTION 'MM60' AND SKIP FIRST SCREEN.
        ENDIF.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
*&---------------------------------------------------------------------*
*
DATA: ok_code      LIKE sy-ucomm, "variable for handling EXIT from report
      ls_fcat      TYPE lvc_s_fcat, "structure / work area with informations about alv layout
      gt_fcat      TYPE lvc_t_fcat, "internal table to store details about alv layout
      g_grid_event TYPE REF TO lcl_for_grid_event, "declaration of object handling events of custom class lcl_for_grid_event
      gs_layout2   TYPE lvc_s_layo, "variable for storing layout
      g_repid      LIKE sy-repid.
*&---------------------------------------------------------------------*
DATA: gs_layout   TYPE disvariant, "major informations about the user-defined layouts
      def_layout  TYPE disvariant,
      spec_layout TYPE disvariant,
      x_layout    TYPE disvariant,
      gs_variant  TYPE disvariant, "chosen layout
      g_exit      TYPE c.
*&---------------------------------------------------------------------*
DATA: matnr TYPE vbap-matnr, "fields types definition
      vbeln TYPE vbak-vbeln,
      kunnr TYPE vbak-kunnr,
      erdat TYPE vbak-erdat.
*&---------------------------------------------------------------------*
* SELECTION SCREEN
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001. "user search criteria

SELECT-OPTIONS: s_matnr FOR matnr,
                s_vbeln FOR vbeln,
                s_kunnr FOR kunnr,
                s_erdat FOR erdat.

SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK vari WITH FRAME TITLE text-002.

PARAMETERS p_vari    LIKE disvariant-variant DEFAULT '/ZDTC_FS1'.

SELECTION-SCREEN END OF BLOCK vari.
*&---------------------------------------------------------------------*
INITIALIZATION.
  g_repid = sy-repid. "variable storing current program name
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.

  MOVE g_repid TO def_layout-report.
  CALL FUNCTION 'LVC_VARIANT_DEFAULT_GET' "function module that reads the default layout.
    EXPORTING
      i_save     = 'A'
    CHANGING
      cs_variant = def_layout
    EXCEPTIONS
      not_found  = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno "error handling
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.

  CLEAR x_layout.
  MOVE g_repid TO x_layout-report.

  CALL FUNCTION 'LVC_VARIANT_F4' "display variant selection dialog box
    EXPORTING
      is_variant = x_layout
      i_save     = 'A'
    IMPORTING
      e_exit     = g_exit
      es_variant = spec_layout
    EXCEPTIONS
      not_found  = 1
      OTHERS     = 2.
  IF sy-subrc NE 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno "error handling
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    IF g_exit NE 'X'.
      p_vari    = spec_layout-variant. "set name of layout on selection screen
    ENDIF.
  ENDIF.
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN.
  CLEAR spec_layout.
  MOVE p_vari  TO spec_layout-variant.
  MOVE g_repid TO spec_layout-report.

  CALL FUNCTION 'LVC_VARIANT_EXISTENCE_CHECK' "checks the existence of a layout variant
    EXPORTING
      i_save        = 'A'
    CHANGING
      cs_variant    = spec_layout
    EXCEPTIONS
      wrong_input   = 1
      not_found     = 2
      program_error = 3
      OTHERS        = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno "error handling
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
*&---------------------------------------------------------------------*
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE' "function module that creates fieldcatalog from structure or internal table
    EXPORTING
      i_structure_name       = 'ZDTC_VBAK_VBAP'
      i_internal_tabname     = 'gt_vbak_vbap'
    CHANGING
      ct_fieldcat            = gt_fcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno "error handling
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
  ls_fcat-hotspot = abap_true.
  MODIFY gt_fcat FROM ls_fcat
    TRANSPORTING hotspot
    WHERE ( fieldname = 'VBELN' ).
  MODIFY gt_fcat FROM ls_fcat
    TRANSPORTING hotspot
    WHERE ( fieldname = 'MATNR' ).
*&---------------------------------------------------------------------*
* GET DATA
*&---------------------------------------------------------------------*
START-OF-SELECTION. "select data from two tables with some conditions, using INNER JOIN
  SELECT b~vbeln  b~vkorg b~netwr b~erdat b~kunnr v~matnr v~arktx v~kwmeng v~meins v~werks
      FROM vbak AS b INNER JOIN vbap AS v ON v~vbeln = b~vbeln
      INTO CORRESPONDING FIELDS OF TABLE gt_vbak_vbap
      WHERE
      b~vbeln IN s_vbeln AND
      v~matnr IN s_matnr AND
      b~kunnr IN s_kunnr AND
      b~erdat IN s_erdat.

END-OF-SELECTION.
*&---------------------------------------------------------------------*
  CLEAR gs_variant.

  gs_layout-report = g_repid.
  gs_variant-report = g_repid.
  IF spec_layout IS INITIAL.
    MOVE-CORRESPONDING def_layout TO gs_variant.
  ELSE.
    MOVE-CORRESPONDING spec_layout TO gs_variant.
  ENDIF.
  CALL SCREEN 100.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno "error handling
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
*&---------------------------------------------------------------------*
* PBO_100 OUTPUT
*&---------------------------------------------------------------------*
MODULE pbo_100 OUTPUT. "process before output handle, it prepares the ALV grid and dislays it in the screen 100
  SET PF-STATUS 'MAIN100'.
  SET TITLEBAR 'RAPORT FILIP'.

  CREATE OBJECT g_grid_event. "create event handling object

  DATA(grid) = NEW cl_gui_alv_grid(
  i_parent = NEW cl_gui_custom_container( container_name = 'ALV' ) "create container and grid objects to display data from internal table filled with selection data
  ).
  SET HANDLER g_grid_event->handle_hotspot_click FOR grid. "set handlers for events on alv grid
  SET HANDLER g_grid_event->handle_double_click FOR grid.
  grid->set_table_for_first_display(
  EXPORTING
    i_structure_name = 'ZDTC_VBAK_VBAP'
    i_save           = 'A' "saving layouts mode
    is_variant       = gs_variant "store saved layout
    CHANGING
      it_fieldcatalog         = gt_fcat
      it_outtab               = gt_vbak_vbap "uses its data to display in alv grid
      EXCEPTIONS
        invalid_parameter_combination = 1 "handle exception
        program_error = 2
        too_many_lines = 3
        OTHERS = 4
        ).
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno "error handling
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
  CALL METHOD cl_gui_control=>set_focus EXPORTING control = grid.
ENDMODULE.
*&---------------------------------------------------------------------*
* PAI_100 INPUT
*&---------------------------------------------------------------------*
MODULE pai_100 INPUT. "process after input, it handles closing of alv grid window
  CALL METHOD cl_gui_cfw=>dispatch.
  CASE ok_code.
    WHEN 'EXIT'.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
*     do nothing
  ENDCASE.
  CLEAR ok_code.
ENDMODULE.
*&---------------------------------------------------------------------*
FORM select_table_vbap USING p_ls_vbap LIKE LINE OF gt_vbak_vbap "select data from captured double click event on one of alv grid's row which contains of VBELN (document number)
                       CHANGING p_gt_vbap LIKE gt_vbap[].


  SELECT * FROM vbap INTO TABLE p_gt_vbap "select all columns from table vbap (which stores informations about items in sales orders)
    WHERE vbeln = p_ls_vbap-vbeln.

*vbeln posnr matnr arktx
ENDFORM.
*&---------------------------------------------------------------------*
FORM create_detail_list. "form to create detailed item list in POP_UP window
  CREATE OBJECT dialogbox_container "create object to store grid2
    EXPORTING
      top      = 150
      left     = 150
      lifetime = cntl_lifetime_dynpro
      caption  = 'Items in selected sales order'(200)
      width    = 800
      height   = 200.
  CREATE OBJECT grid2 "create object to display detailed item list
    EXPORTING
      i_parent = dialogbox_container.
  SET HANDLER g_grid_event->handle_close FOR dialogbox_container. "handler for closing POP_UP window

  gs_layout2-grid_title = space.
  CALL METHOD grid2->set_table_for_first_display
    EXPORTING
      i_structure_name = 'VBAP'
      is_layout        = gs_layout2
    CHANGING
      it_outtab        = gt_vbap.

  CALL METHOD cl_gui_control=>set_focus EXPORTING control = grid2.

ENDFORM.
*&---------------------------------------------------------------------*
