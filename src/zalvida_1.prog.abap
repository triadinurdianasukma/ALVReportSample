*&---------------------------------------------------------------------*
*& Report zalvida_1
*&---------------------------------------------------------------------*
*& Simple ALV IDA with table selection and filter (selection parameter) and local class
*&---------------------------------------------------------------------*
REPORT zalvida_1.

TABLES : zdemo_soh.

CLASS lcx_general_msg DEFINITION
  INHERITING FROM cx_static_check
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_t100_message .
    INTERFACES if_t100_dyn_msg .

    ALIASES msgty
      FOR if_t100_dyn_msg~msgty .

    METHODS constructor
      IMPORTING
        !textid   LIKE if_t100_message=>t100key OPTIONAL
        !previous LIKE previous OPTIONAL
        !msgty    TYPE symsgty OPTIONAL .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcx_general_msg IMPLEMENTATION.
  METHOD constructor.
    CALL METHOD super->constructor
      EXPORTING
        previous = previous.
    me->msgty = msgty .
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_main DEFINITION CREATE PUBLIC FINAL.

  PUBLIC SECTION.
    CLASS-DATA : gs_persistence_key TYPE if_salv_gui_layout_persistence=>ys_persistence_key.

    CLASS-METHODS :
      select_layout
        IMPORTING im_persistence_key TYPE if_salv_gui_layout_persistence=>ys_persistence_key
        RETURNING VALUE(return)      TYPE slis_vari.


    TYPES : BEGIN OF ty_selection,
              salesordernumber TYPE RANGE OF zdemo_soh-salesorder,
              bpnumber         TYPE RANGE OF zdemo_soh-businesspartner,
              layout           TYPE if_salv_gui_layout_persistence=>y_layout_name,
            END OF ty_selection.

    DATA: go_alv_ida        TYPE REF TO if_salv_gui_table_ida,
          go_alv_ida_layout TYPE REF TO if_salv_gui_layout_ida.

    METHODS :
      run
        IMPORTING im_selection TYPE ty_selection
        RAISING   cx_static_check.
  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS :
      check_parameter
        IMPORTING im_selection  TYPE ty_selection
        RETURNING VALUE(return) TYPE abap_bool,
      set_filter
        IMPORTING im_selection  TYPE ty_selection
        RETURNING VALUE(return) TYPE if_salv_service_types=>yt_named_ranges.



ENDCLASS.

CLASS lcl_main IMPLEMENTATION.

  METHOD run.
    TRY.
        "Create Interface
        go_alv_ida = cl_salv_gui_table_ida=>create( iv_table_name = 'ZDEMO_SOH' ).

        "Set title
        go_alv_ida->display_options( )->set_title( 'Sales Data' ).

        "Check Selection Parameter
        DATA(retcode) = check_parameter( im_selection ).
        CHECK retcode IS INITIAL.

        "Set Filter
        DATA(lt_range) = set_filter( im_selection ).

        "Filter Data
        go_alv_ida->set_select_options(
          EXPORTING
            it_ranges    = lt_range
        ).

        "Set Layout
        go_alv_ida->layout_persistence( )->set_persistence_options(
          EXPORTING
            is_persistence_key           = gs_persistence_key
        ).
        go_alv_ida->toolbar(  )->enable_listbox_for_layouts(  ).
        IF im_selection-layout IS NOT INITIAL.
          go_alv_ida->layout_persistence( )->set_start_layout( im_selection-layout ).
        ENDIF.

        "Display

        go_alv_ida->fullscreen( )->display( ).
      CATCH lcx_general_msg INTO DATA(lo_general_msg).
        RAISE EXCEPTION lo_general_msg.
    ENDTRY.

  ENDMETHOD.

  METHOD select_layout.
    cl_salv_gui_grid_utils_ida=>f4_for_layouts( EXPORTING is_persistence_key = im_persistence_key
                                              IMPORTING es_selected_layout = DATA(ls_selected_layout) ).
    return = ls_selected_layout-name.
  ENDMETHOD.

  METHOD check_parameter.
    "Check parameter and other requirement like Auth Object, etc.
    "in this sample, no checking parameter required
    return = abap_false.

  ENDMETHOD.

  METHOD set_filter.
    DATA(lo_range_collector) = NEW cl_salv_range_tab_collector( ).

    IF im_selection-salesordernumber[] IS NOT INITIAL.
      lo_range_collector->add_ranges_for_name(
        EXPORTING
          iv_name   = 'SALESORDER'
          it_ranges = im_selection-salesordernumber[]
      ).
    ENDIF.

    IF im_selection-bpnumber IS NOT INITIAL.
      lo_range_collector->add_ranges_for_name(
        EXPORTING
          iv_name   = 'BUSINESSPARTNER'
          it_ranges = im_selection-bpnumber[]
      ).
    ENDIF.

    lo_range_collector->get_collected_ranges(
      IMPORTING
        et_named_ranges = return
    ).

  ENDMETHOD.

ENDCLASS.


*----------------------------------------------------------------------*
* S E L E C T I O N - S C R E E N
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK se1 WITH FRAME TITLE TEXT-se1.
SELECT-OPTIONS : s_ordnum FOR zdemo_soh-salesorder,
                 s_bpnum FOR zdemo_soh-businesspartner.
SELECTION-SCREEN END OF BLOCK se1.

SELECTION-SCREEN BEGIN OF BLOCK se2 WITH FRAME TITLE TEXT-se2.
PARAMETERS: p_layout TYPE if_salv_gui_layout_persistence=>y_layout_name.
SELECTION-SCREEN END OF BLOCK se2.

*----------------------------------------------------------------------*
* A T   S E L E C T I O N - S C R E E N
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_layout.
  p_layout = lcl_main=>select_layout( lcl_main=>gs_persistence_key ).

*----------------------------------------------------------------------*
INITIALIZATION.
*----------------------------------------------------------------------*
*   Define the Layout Key
  lcl_main=>gs_persistence_key-report_name = sy-repid.

*----------------------------------------------------------------------*
* S T A R T - O F - S E L E C T I O N
*----------------------------------------------------------------------*
START-OF-SELECTION.
  DATA(lo_report) = NEW lcl_main( ).
  lo_report->run( im_selection = VALUE lcl_main=>ty_selection( salesordernumber = s_ordnum[]
                                                               bpnumber         = s_bpnum[]
                                                               layout           = p_layout )
                                                             ).

END-OF-SELECTION.
