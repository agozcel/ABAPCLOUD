*&---------------------------------------------------------------------*
*& Report ZTEST_GUN02
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZTEST_GUN02.



*-----------------------------------------------------------------------
*... This report demonstrates how to add application specific funtions
*    to the ALV OM
*      - cl_salv_table (Fullscreen Grid, Fullscreen List, Grid)
*
*    If the table ALV_T_T2 is empty, please create data for the demo
*    by running report BCALV_GENERATE_ALV_T_T2
*
* §1   select data into global output table
*
* §2   create ALV Table
*      create instance of cl_salv_table for displaying a list of your
*      output table
* §2.1 create a ALV List by setting the parameter LIST_DISPLAY of the
*      constructor cl_salv_table to 'X'
* §2.2 create a ALV Fullscreen Grid by not setting the parameter
*      LIST_DISPLAY of the constructor cl_salv_table to 'X'
* §2.3 create a Grid by creating a Dynpro with a container in it. In
*      PBO check if the container has been instantiated. If the
*      container has not yet been instantiated then this is the first
*      call:
*            (1) create the container
*            (2) create an instance of cl_salv_table
*
* §3   Functions
* §3.1 activate ALV generic Functions
* §3.2 include own functions
*
* §5   Event Handler
*      define a handler for the events of cl_salv_table
* §5.1 define a local class for handling events of cl_salv_table
*      define methods for the events which are to be handled
* §5.2 implement the defined methods for handling the events of
*      cl_salv_table
*
* §6   Events
*      register to events of cl_salv_table
* §6.1 register to the event USER_COMMAND of cl_salv_table
*      for receiving information when own functions has been selected
*
* §7   Display
*      display the configurated ALV Table by calling the method
*      display of cl_salv_table.
*-----------------------------------------------------------------------

include <color>.

include <icon>.
include <symbol>.

types: begin of g_type_s_test,
         amount  type i,
         repid   type syrepid,
         display type i,
         dynamic type sap_bool,
       end of g_type_s_test.

constants: gc_true  type sap_bool value 'X',

           begin of gc_s_display,
             list       type i value 1,
             fullscreen type i value 2,
             grid       type i value 3,
           end   of gc_s_display.

*... §5 Definition is later
class lcl_handle_events definition deferred.

data: gs_test type g_type_s_test.

data: gt_outtab type standard table of alv_t_t2.

data: gr_table   type ref to cl_salv_table.

data: gr_container type ref to cl_gui_custom_container.

*... §5 object for handling the events of cl_salv_table
data: gr_events type ref to lcl_handle_events.

data: g_okcode type syucomm.

*---------------------------------------------------------------------*
*       CLASS lcl_handle_events DEFINITION
*---------------------------------------------------------------------*
* §5.1 define a local class for handling events of cl_salv_table
*---------------------------------------------------------------------*
class lcl_handle_events definition.
  public section.
    methods:
      on_user_command for event added_function of cl_salv_events
        importing e_salv_function,
      on_before_user_command for event before_salv_function of cl_salv_events
        importing e_salv_function,
      on_after_user_command for event after_salv_function of cl_salv_events
        importing e_salv_function.
endclass.                    "lcl_handle_events DEFINITION

*---------------------------------------------------------------------*
*       CLASS lcl_handle_events IMPLEMENTATION
*---------------------------------------------------------------------*
* §5.2 implement the events for handling the events of cl_salv_table
*---------------------------------------------------------------------*
class lcl_handle_events implementation.
  method on_user_command.
    perform show_function_info using e_salv_function text-i08.
  endmethod.                    "on_user_command

  method on_before_user_command.
    perform show_function_info using e_salv_function text-i09.
  endmethod.                    "on_before_user_command

  method on_after_user_command.
    perform show_function_info using e_salv_function text-i10.
  endmethod.                    "on_after_user_command

endclass.                    "lcl_handle_events IMPLEMENTATION

*----------------------------------------------------------------------*
* SELECTION-SCREEN - for demonstration purposes only                   *
*----------------------------------------------------------------------*
selection-screen begin of block gen with frame.
parameters:
p_amount type i default 30.
selection-screen end of block gen.

selection-screen begin of block dsp with frame.
parameters:
p_full   radiobutton group dsp,
p_list   radiobutton group dsp,
p_grid   radiobutton group dsp.
selection-screen end of block dsp.

*----------------------------------------------------------------------*
* START-OF-SELECTION                                                   *
*----------------------------------------------------------------------*
start-of-selection.
  gs_test-amount = p_amount.
  gs_test-repid = sy-repid.

  case gc_true.
    when p_list.
      gs_test-display = gc_s_display-list.
    when p_full.
      gs_test-display = gc_s_display-fullscreen.
    when p_grid.
      gs_test-display = gc_s_display-grid.
  endcase.

*... §1 select data into global output table
  perform select_data.

*----------------------------------------------------------------------*
* END-OF-SELECTION                                                     *
*----------------------------------------------------------------------*
end-of-selection.
  case gs_test-display.
    when gc_s_display-fullscreen.
      perform display_fullscreen.

    when gc_s_display-grid.
      perform display_grid.

    when gc_s_display-list.
      perform display_list.
  endcase.

*&---------------------------------------------------------------------*
*&      Form  select_data
*&---------------------------------------------------------------------*
* §1 select data into your global output table
*----------------------------------------------------------------------*
form select_data.

  select * from alv_t_t2 into corresponding fields of table gt_outtab
        up to gs_test-amount rows.                          "#EC *

endform.                    " select_data

*&---------------------------------------------------------------------*
*&      Form  display_fullscreen
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form display_fullscreen .

*... §2 create an ALV table
*    §2.2 just create an instance and do not set LIST_DISPLAY for
*         displaying the data as a Fullscreen Grid
  try.
      cl_salv_table=>factory(
        importing
          r_salv_table = gr_table
        changing
          t_table      = gt_outtab ).
    catch cx_salv_msg.                                  "#EC NO_HANDLER
  endtry.

*... §3 Functions
*... §3.1 activate ALV generic Functions
*... §3.2 include own functions by setting own status
  gr_table->set_screen_status(
    pfstatus      =  'SALV_STANDARD'
    report        =  gs_test-repid
    set_functions = gr_table->c_functions_all ).

*... set the columns technical
  data: lr_columns type ref to cl_salv_columns,
        lr_column  type ref to cl_salv_column_table.

  lr_columns = gr_table->get_columns( ).
  lr_columns->set_optimize( gc_true ).

  perform set_columns_technical using lr_columns.

*... §4 set hotspot column
  try.
      lr_column ?= lr_columns->get_column( 'CARRID' ).
      lr_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
    catch cx_salv_not_found.                            "#EC NO_HANDLER
  endtry.

*... §6 register to the events of cl_salv_table
  data: lr_events type ref to cl_salv_events_table.

  lr_events = gr_table->get_event( ).

  create object gr_events.

*... §6.1 register to the event USER_COMMAND
  set handler gr_events->on_user_command for lr_events.

  set handler gr_events->on_before_user_command for lr_events.

  set handler gr_events->on_after_user_command for lr_events.
*... set list title
  data: lr_display_settings type ref to cl_salv_display_settings,
        l_title type lvc_title.

  l_title = text-t01.
  lr_display_settings = gr_table->get_display_settings( ).
  lr_display_settings->set_list_header( l_title ).

*... §7 display the table
  gr_table->display( ).

endform.                    " display_fullscreen

*&---------------------------------------------------------------------*
*&      Form  display_list
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form display_list .

*... §2 create an ALV table
*    §2.1 set LIST_DISPLAY to 'X' for displaying an ALV List
  try.
      cl_salv_table=>factory(
        exporting
          list_display = gc_true
        importing
          r_salv_table = gr_table
        changing
          t_table      = gt_outtab ).
    catch cx_salv_msg.                                  "#EC NO_HANDLER
  endtry.

*... §3 Functions
*... §3.1 activate ALV generic Functions
*... §3.2 include own functions by setting own status
  gr_table->set_screen_status(
    pfstatus      =  'SALV_STANDARD'
    report        =  gs_test-repid
    set_functions = gr_table->c_functions_all ).

*... set the columns technical
  data: lr_columns type ref to cl_salv_columns,
        lr_column  type ref to cl_salv_column_table.

  lr_columns = gr_table->get_columns( ).
  lr_columns->set_optimize( gc_true ).

  perform set_columns_technical using lr_columns.

*... §4 set hotspot column
  try.
      lr_column ?= lr_columns->get_column( 'CARRID' ).
      lr_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
    catch cx_salv_not_found.                            "#EC NO_HANDLER
  endtry.

*... §6 register to the events of cl_salv_table
  data: lr_events type ref to cl_salv_events_table.

  lr_events = gr_table->get_event( ).

  create object gr_events.

*... §6.1 register to the event USER_COMMAND
  set handler gr_events->on_user_command for lr_events.

  set handler gr_events->on_before_user_command for lr_events.

  set handler gr_events->on_after_user_command for lr_events.
*... set list title
  data: lr_display_settings type ref to cl_salv_display_settings,
        l_title type lvc_title.

  l_title = text-t01.
  lr_display_settings = gr_table->get_display_settings( ).
  lr_display_settings->set_list_header( l_title ).

*... §7 display the table
  gr_table->display( ).

endform.                    " display_list

*&--------------------------------------------------------------------*
*&      Form  display_grid
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
form display_grid.

  call screen 100.

endform.                    "display_grid

*&---------------------------------------------------------------------*
*&      Module  d0100_pbo  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module d0100_pbo output.
  perform d0100_pbo.
endmodule.                 " d0100_pbo  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  d0100_pai  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module d0100_pai input.
  perform d0100_pai.
endmodule.                 " d0100_pai  INPUT

*&---------------------------------------------------------------------*
*&      Form  d0100_pbo
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form d0100_pbo .

  set pf-status 'D0100'.

  if gr_container is not bound.
    if cl_salv_table=>is_offline( ) eq if_salv_c_bool_sap=>false.
      create object gr_container
        exporting
          container_name = 'CONTAINER'.
    endif.

*... §2 create an ALV table
    try.
        cl_salv_table=>factory(
          exporting
            r_container    = gr_container
            container_name = 'CONTAINER'
          importing
            r_salv_table   = gr_table
          changing
            t_table        = gt_outtab ).
      catch cx_salv_msg.                                "#EC NO_HANDLER
    endtry.

*... §3.1 activate ALV generic Functions
    data: lr_functions type ref to cl_salv_functions,
          l_text       type string,
          l_icon       type string.

    lr_functions = gr_table->get_functions( ).
    lr_functions->set_all( gc_true ).

*... §3.2 include own functions
    l_text = text-b01.
    l_icon = icon_complete.
    try.
      lr_functions->add_function(
        name     = 'MYFUNCTION'
        icon     = l_icon
        text     = l_text
        tooltip  = l_text
        position = if_salv_c_function_position=>right_of_salv_functions ).
      catch cx_salv_existing cx_salv_wrong_call.
    endtry.

*... set the columns technical
    data: lr_columns type ref to cl_salv_columns,
          lr_column  type ref to cl_salv_column_table.

    lr_columns = gr_table->get_columns( ).
    lr_columns->set_optimize( gc_true ).

    perform set_columns_technical using lr_columns.

*... §4 set hotspot column
    try.
        lr_column ?= lr_columns->get_column( 'CARRID' ).
        lr_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
      catch cx_salv_not_found.                          "#EC NO_HANDLER
    endtry.

*... §6 register to the events of cl_salv_table
    data: lr_events type ref to cl_salv_events_table.

    lr_events = gr_table->get_event( ).

    create object gr_events.

*... §6.1 register to the event USER_COMMAND
    set handler gr_events->on_user_command for lr_events.

    set handler gr_events->on_before_user_command for lr_events.

    set handler gr_events->on_after_user_command for lr_events.

*... set list title
    data: lr_display_settings type ref to cl_salv_display_settings,
          l_title type lvc_title.

    l_title = text-t01.
    lr_display_settings = gr_table->get_display_settings( ).
    lr_display_settings->set_list_header( l_title ).

*... §7 display the table
    gr_table->display( ).
  endif.

endform.                                                    " d0100_pbo

*&---------------------------------------------------------------------*
*&      Form  d0100_pai
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form d0100_pai .

  case g_okcode.
    when 'BACK' or 'EXIT' or 'CANC'.
      set screen 0.
      leave screen.
  endcase.

endform.                                                    " d0100_pai

*&---------------------------------------------------------------------*
*&      Form  show_function_info
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form show_function_info using i_function type salv_de_function
                              i_text     type string.

  data: l_string type string.

  concatenate i_text i_function into l_string separated by space.

  message i000(0k) with l_string.

endform.                    " show_function_info

*&---------------------------------------------------------------------*
*&      Form  set_columns_technical
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form set_columns_technical using ir_columns type ref to cl_salv_columns.

  data: lr_column type ref to cl_salv_column.

  try.
      lr_column = ir_columns->get_column( 'MANDT' ).
      lr_column->set_technical( if_salv_c_bool_sap=>true ).
    catch cx_salv_not_found.                            "#EC NO_HANDLER
  endtry.

  try.
      lr_column = ir_columns->get_column( 'FLOAT_FI' ).
      lr_column->set_technical( if_salv_c_bool_sap=>true ).
    catch cx_salv_not_found.                            "#EC NO_HANDLER
  endtry.

  try.
      lr_column = ir_columns->get_column( 'STRING_F' ).
      lr_column->set_technical( if_salv_c_bool_sap=>true ).
    catch cx_salv_not_found.                            "#EC NO_HANDLER
  endtry.

  try.
      lr_column = ir_columns->get_column( 'XSTRING' ).
      lr_column->set_technical( if_salv_c_bool_sap=>true ).
    catch cx_salv_not_found.                            "#EC NO_HANDLER
  endtry.

  try.
      lr_column = ir_columns->get_column( 'INT_FIEL' ).
      lr_column->set_technical( if_salv_c_bool_sap=>true ).
    catch cx_salv_not_found.                            "#EC NO_HANDLER
  endtry.

  try.
      lr_column = ir_columns->get_column( 'HEX_FIEL' ).
      lr_column->set_technical( if_salv_c_bool_sap=>true ).
    catch cx_salv_not_found.                            "#EC NO_HANDLER
  endtry.

  try.
      lr_column = ir_columns->get_column( 'DROPDOWN' ).
      lr_column->set_technical( if_salv_c_bool_sap=>true ).
    catch cx_salv_not_found.                            "#EC NO_HANDLER
  endtry.

  try.
      lr_column = ir_columns->get_column( 'TAB_INDEX' ).
      lr_column->set_technical( if_salv_c_bool_sap=>true ).
    catch cx_salv_not_found.                            "#EC NO_HANDLER
  endtry.

endform.                    " set_columns_technical(
