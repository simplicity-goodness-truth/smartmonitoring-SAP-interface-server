class ZCL_ZAI_OPS_PROVIDER_DPC_EXT definition
  public
  inheriting from ZCL_ZAI_OPS_PROVIDER_DPC
  create public .

public section.

  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~CREATE_STREAM
    redefinition .
protected section.

  methods MAI_SCOPE_GET_ENTITYSET
    redefinition .
PRIVATE SECTION.


  TYPES:
    BEGIN OF ty_ext_mai_ids_map_tt,
      ext_context_id        TYPE ac_guid,
      mai_context_id        TYPE ac_guid,
      ext_event_type_id TYPE ac_guid,
      mai_event_type_id TYPE ac_guid,
    END OF ty_ext_mai_ids_map_tt .

  CLASS-DATA lt_ext_mai_ids_map TYPE STANDARD TABLE OF ty_ext_mai_ids_map_tt WITH KEY ext_context_id ext_event_type_id.
  CLASS-DATA ls_ext_mai_ids_map TYPE ty_ext_mai_ids_map_tt .

  CLASS-METHODS get_metric_values
    IMPORTING
      !iv_context_id    TYPE ac_guid
      !iv_context_name  TYPE ac_context_name
      !iv_event_type_id TYPE ac_guid
      !iv_mname         TYPE ac_name
    EXPORTING
      !et_snapshot      TYPE zai_ops_mai_snapshot .
ENDCLASS.



CLASS ZCL_ZAI_OPS_PROVIDER_DPC_EXT IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ZAI_OPS_PROVIDER_DPC_EXT->/IWBEP/IF_MGW_APPL_SRV_RUNTIME~CREATE_STREAM
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ENTITY_NAME                 TYPE        STRING(optional)
* | [--->] IV_ENTITY_SET_NAME             TYPE        STRING(optional)
* | [--->] IV_SOURCE_NAME                 TYPE        STRING(optional)
* | [--->] IS_MEDIA_RESOURCE              TYPE        TY_S_MEDIA_RESOURCE
* | [--->] IT_KEY_TAB                     TYPE        /IWBEP/T_MGW_NAME_VALUE_PAIR(optional)
* | [--->] IT_NAVIGATION_PATH             TYPE        /IWBEP/T_MGW_NAVIGATION_PATH(optional)
* | [--->] IV_SLUG                        TYPE        STRING
* | [--->] IO_TECH_REQUEST_CONTEXT        TYPE REF TO /IWBEP/IF_MGW_REQ_ENTITY_C(optional)
* | [<---] ER_ENTITY                      TYPE REF TO DATA
* | [!CX!] /IWBEP/CX_MGW_BUSI_EXCEPTION
* | [!CX!] /IWBEP/CX_MGW_TECH_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD /iwbep/if_mgw_appl_srv_runtime~create_stream.


    DATA lv_request_string TYPE string.

    TYPES: BEGIN OF ty_response_tt,
             response TYPE string,
           END OF ty_response_tt.

    DATA lt_response TYPE STANDARD TABLE OF ty_response_tt.
    DATA ls_response TYPE ty_response_tt.

    DATA: lr_json  TYPE REF TO /ui2/cl_json.

    TYPES: BEGIN OF ty_snapshot_request_tt,
             context_id    TYPE ac_guid,
             event_type_id TYPE ac_guid,
             context_name  TYPE ac_context_name,
             mname         TYPE ac_name,
           END OF ty_snapshot_request_tt.

    DATA lt_snapshot_request TYPE STANDARD TABLE OF ty_snapshot_request_tt.
    DATA ls_snapshot_request TYPE ty_snapshot_request_tt.

    FIELD-SYMBOLS <ls_snapshot_request> LIKE LINE OF lt_snapshot_request.

*    TYPES: BEGIN OF ty_raw_request_tt,
*             value TYPE string,
*           END OF ty_raw_request_tt.
*
*    DATA lt_raw_request TYPE STANDARD TABLE OF ty_raw_request_tt.
*    DATA ls_raw_request TYPE ty_raw_request_tt.
*
*
*    DATA lt_snapshot_record  TYPE TABLE OF char1024.
*    FIELD-SYMBOLS <ls_snapshot_record> LIKE LINE OF lt_snapshot_record.
*
*    TYPES: BEGIN OF ty_request_line_tt,
*             value TYPE char200,
*           END OF ty_request_line_tt.
*
*    DATA lt_request_line TYPE STANDARD TABLE OF ty_request_line_tt.

    DATA lv_backend_response TYPE string.

    DATA lt_snapshot TYPE STANDARD TABLE OF zai_ops_mai_snapshot.
    DATA ls_snapshot TYPE zai_ops_mai_snapshot.

    DATA lv_result TYPE string.
    DATA lv_xstring TYPE xstring.


    DATA lv_exec_time_start TYPE i.
    DATA lv_exec_time_end TYPE i.


    GET RUN TIME FIELD lv_exec_time_start.


    " Picking up payload from is_media_resource-value in xstring and transforming it into string

    lv_xstring = is_media_resource-value.


    cl_bcs_convert=>xstring_to_string(
        EXPORTING
          iv_xstr   = lv_xstring
          iv_cp     =  1100                " SAP character set identification
        RECEIVING
          rv_string = lv_request_string
      ).
    IF sy-subrc = 0.

      " Parsing of response

      " Replacing  quotes

      REPLACE ALL OCCURRENCES OF '''' IN lv_request_string WITH '"'.

      " Removing special characters

      REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf(1) IN lv_request_string WITH space.

      REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf+1(1) IN lv_request_string WITH space.

      CONDENSE lv_request_string.

      cl_fdt_json=>json_to_data( EXPORTING iv_json = lv_request_string CHANGING ca_data = lt_snapshot_request ).


*      CREATE OBJECT lr_json.
*
*      TRY .
*          lr_json->deserialize_int( EXPORTING json = lv_request_string CHANGING data = ls_raw_request ).
*        CATCH cx_sy_move_cast_error INTO DATA(lo_move_cast_error) .
*      ENDTRY.
*
*      IF sy-subrc = 0.
*
*        SPLIT ls_raw_request-value AT '^^' INTO TABLE lt_snapshot_record.
*
*        LOOP AT lt_snapshot_record ASSIGNING <ls_snapshot_record>.
*
*          CLEAR ls_snapshot_request.
*
*          SPLIT <ls_snapshot_record> AT ';' INTO TABLE lt_request_line.
*
*          LOOP AT lt_request_line ASSIGNING FIELD-SYMBOL(<ls_request_line>).
*
*            IF ( ls_snapshot_request-context_id IS INITIAL ).
*              ls_snapshot_request-context_id = <ls_request_line>-value.
*              CONTINUE.
*            ENDIF.
*
*            IF ( ls_snapshot_request-event_type_id IS INITIAL ).
*              ls_snapshot_request-event_type_id = <ls_request_line>-value.
*              CONTINUE.
*            ENDIF.
*
*            IF ( ls_snapshot_request-context_name IS INITIAL ).
*              ls_snapshot_request-context_name = <ls_request_line>-value.
*              CONTINUE.
*            ENDIF.
*
*            IF ( ls_snapshot_request-mname IS INITIAL ).
*              ls_snapshot_request-mname = <ls_request_line>-value.
*              CONTINUE.
*            ENDIF.
*
*          ENDLOOP. " LOOP AT lt_request_line ASSIGNING FIELD-SYMBOL(<ls_request_line>)
*
*          IF ( ls_snapshot_request IS NOT INITIAL ).
*
*            APPEND ls_snapshot_request TO lt_snapshot_request.
*
*          ENDIF. " IF ( ls_snapshot_request IS NOT INITIAL )
*
*        ENDLOOP. " LOOP AT lt_snapshot_record ASSIGNING <ls_snapshot_record>
*
*      ENDIF.

      IF lt_snapshot_request IS NOT INITIAL.

        LOOP AT lt_snapshot_request ASSIGNING <ls_snapshot_request>.
          CLEAR ls_snapshot.

          zcl_zai_ops_provider_dpc_ext=>get_metric_values(
             EXPORTING
                iv_context_id = <ls_snapshot_request>-context_id
                iv_event_type_id = <ls_snapshot_request>-event_type_id
                iv_context_name = <ls_snapshot_request>-context_name
                iv_mname = <ls_snapshot_request>-mname
             IMPORTING
                et_snapshot = ls_snapshot ).

          IF ls_snapshot-value_count > 0.

            ls_snapshot-context_id = <ls_snapshot_request>-context_id.
            ls_snapshot-event_type_id = <ls_snapshot_request>-event_type_id.
            ls_snapshot-context_name = <ls_snapshot_request>-context_name.
            ls_snapshot-mname = <ls_snapshot_request>-mname.

            APPEND ls_snapshot TO lt_snapshot.

          ENDIF. " IF ls_snapshot-value_count > 0
        ENDLOOP. "  LOOP AT lt_snapshot_request ASSIGNING <ls_snapshot_request>

        GET RUN TIME FIELD lv_exec_time_end.

        CLEAR ls_snapshot.
        ls_snapshot-context_id = 'server_processing_time'.
        ls_snapshot-value_sum = ( ( lv_exec_time_end - lv_exec_time_start ) / 1000 ).
        ls_snapshot-value_count = 1.
        "ls_snapshot-VALUE_TIMESTAMP = '00000000000000'.

        APPEND ls_snapshot TO lt_snapshot.


        DATA(o_writer_itab) = cl_sxml_string_writer=>create( type = if_sxml=>co_xt_json ).

        CALL TRANSFORMATION id SOURCE values = lt_snapshot RESULT XML o_writer_itab.

        cl_abap_conv_in_ce=>create( )->convert( EXPORTING
                                                  input = o_writer_itab->get_output( )
                                                IMPORTING
                                                  data = lv_result ).

        " Filling response with metrics calculation

       " TRANSLATE lv_result TO LOWER CASE.

        ls_response-response =  lv_result.
*
        "Fill the export parameter er_entity accordingly

        copy_data_to_ref(
          EXPORTING
             is_data = ls_response
       	  CHANGING
             cr_data = er_entity ).


      ENDIF. " IF lt_snapshot_request IS NOT INITIAL.

      " Preparing a resulted JSON array


    ENDIF. " IF sy-subrc = 0

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_ZAI_OPS_PROVIDER_DPC_EXT=>GET_METRIC_VALUES
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_CONTEXT_ID                  TYPE        AC_GUID
* | [--->] IV_CONTEXT_NAME                TYPE        AC_CONTEXT_NAME
* | [--->] IV_EVENT_TYPE_ID               TYPE        AC_GUID
* | [--->] IV_MNAME                       TYPE        AC_NAME
* | [<---] ET_SNAPSHOT                    TYPE        ZAI_OPS_MAI_SNAPSHOT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_metric_values.

    DATA lv_context_id TYPE ac_guid.
    DATA lv_event_type_id TYPE ac_guid.


    " Checking if new MAI id was already recorded within current snapshot request

    CLEAR ls_ext_mai_ids_map.

    READ TABLE lt_ext_mai_ids_map
      WITH TABLE KEY ext_context_id = iv_context_id
                     ext_event_type_id = iv_event_type_id
      INTO ls_ext_mai_ids_map.

    IF ls_ext_mai_ids_map-mai_event_type_id IS INITIAL.

      lv_context_id = iv_context_id.
      lv_event_type_id = iv_event_type_id.

    ELSE.

     lv_context_id = ls_ext_mai_ids_map-mai_context_id.
     lv_event_type_id = ls_ext_mai_ids_map-mai_event_type_id.

    ENDIF.

    SELECT  value_timestamp value_max value_min value_sum value_count FROM ace_db_event
      INTO ( et_snapshot-value_timestamp, et_snapshot-value_max, et_snapshot-value_min, et_snapshot-value_sum, et_snapshot-value_count  )
        UP TO 1 ROWS
        WHERE context_id  = lv_context_id
        AND event_type_id = lv_event_type_id
       ORDER BY save_timestamp DESCENDING.
    ENDSELECT.

    IF sy-subrc <> 0 .

      SELECT SINGLE context_id FROM v_acentrypoints
        INTO lv_context_id
        WHERE context_name = iv_context_name
        AND ac_variant = 'A'.

      IF sy-subrc = 0.

        SELECT SINGLE event_type_id FROM acmetricdir INTO lv_event_type_id
          WHERE name = iv_mname AND
          ac_variant = 'A' AND
          context_id = lv_context_id
          AND event_class eq cl_alrt_cfg_constants=>ac_entity_metric.

        IF sy-subrc = 0.

            SELECT  value_timestamp value_max value_min value_sum value_count FROM ace_db_event
              INTO ( et_snapshot-value_timestamp, et_snapshot-value_max, et_snapshot-value_min, et_snapshot-value_sum, et_snapshot-value_count  )
                UP TO 1 ROWS
                WHERE context_id  = lv_context_id
                AND event_type_id = lv_event_type_id
                ORDER BY save_timestamp DESCENDING.
            ENDSELECT.

            IF sy-subrc = 0.

              ls_ext_mai_ids_map-ext_context_id = iv_context_id.
              ls_ext_mai_ids_map-mai_context_id = lv_context_id.
              ls_ext_mai_ids_map-ext_event_type_id = iv_event_type_id.
              ls_ext_mai_ids_map-mai_event_type_id = lv_event_type_id.

              APPEND ls_ext_mai_ids_map TO lt_ext_mai_ids_map.

           ENDIF. " IF sy-subrc = 0
        ENDIF. " IF sy-subrc = 0
      ENDIF. " IF sy-subrc = 0
    ENDIF. " IF sy-subrc <

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_ZAI_OPS_PROVIDER_DPC_EXT->MAI_SCOPE_GET_ENTITYSET
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ENTITY_NAME                 TYPE        STRING
* | [--->] IV_ENTITY_SET_NAME             TYPE        STRING
* | [--->] IV_SOURCE_NAME                 TYPE        STRING
* | [--->] IT_FILTER_SELECT_OPTIONS       TYPE        /IWBEP/T_MGW_SELECT_OPTION
* | [--->] IS_PAGING                      TYPE        /IWBEP/S_MGW_PAGING
* | [--->] IT_KEY_TAB                     TYPE        /IWBEP/T_MGW_NAME_VALUE_PAIR
* | [--->] IT_NAVIGATION_PATH             TYPE        /IWBEP/T_MGW_NAVIGATION_PATH
* | [--->] IT_ORDER                       TYPE        /IWBEP/T_MGW_SORTING_ORDER
* | [--->] IV_FILTER_STRING               TYPE        STRING
* | [--->] IV_SEARCH_STRING               TYPE        STRING
* | [--->] IO_TECH_REQUEST_CONTEXT        TYPE REF TO /IWBEP/IF_MGW_REQ_ENTITYSET(optional)
* | [<---] ET_ENTITYSET                   TYPE        ZCL_ZAI_OPS_PROVIDER_MPC=>TT_MAI_SCOPE
* | [<---] ES_RESPONSE_CONTEXT            TYPE        /IWBEP/IF_MGW_APPL_SRV_RUNTIME=>TY_S_MGW_RESPONSE_CONTEXT
* | [!CX!] /IWBEP/CX_MGW_BUSI_EXCEPTION
* | [!CX!] /IWBEP/CX_MGW_TECH_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD mai_scope_get_entityset.
 


    DATA lt_v_acentrypoints TYPE STANDARD TABLE OF v_acentrypoints.
    DATA lt_mo_selection LIKE lt_v_acentrypoints.
    DATA ls_mo_selection TYPE v_acentrypoints.
    DATA lt_acmetricdir TYPE STANDARD TABLE OF acmetricdir.
    DATA lv_db_tab_name TYPE ddobjname.
    DATA lv_greentoyellow TYPE ac_green_to_yellow.
    DATA lv_yellowtored TYPE ac_yellow_to_red.

    TYPES: BEGIN OF ty_context_names,
             mo_name TYPE ac_context_name,
           END OF ty_context_names.

    DATA: lt_context_names TYPE TABLE OF ty_context_names .



    FIELD-SYMBOLS <context_name_line> LIKE LINE OF lt_context_names.

    TYPES: BEGIN OF ty_metric_names,
             metric_name TYPE ac_name,
           END OF ty_metric_names.

    DATA: lt_metric_names TYPE TABLE OF ty_metric_names .

    FIELD-SYMBOLS <metric_name> LIKE LINE OF lt_metric_names.

    DATA: lr_context_name TYPE RANGE OF ty_context_names-mo_name,
          wa_context_name LIKE LINE OF lr_context_name.

    DATA: lr_metric_name TYPE RANGE OF ty_metric_names-metric_name,
          wa_metric_name LIKE LINE OF lr_metric_name.

    FIELD-SYMBOLS <metricdir> LIKE LINE OF lt_acmetricdir.

    DATA lv_mo_name TYPE v_acentrypoints-context_name.
    DATA lv_short_text TYPE ac_text_100.


    DATA lv_context_name_list TYPE string.
    DATA lv_metric_name_list TYPE string.
    DATA lv_tech_scenario TYPE ac_technical_scenario.

    " Data types for filters

    DATA: lt_filters TYPE  /iwbep/t_mgw_select_option,
          ls_filter  TYPE  /iwbep/s_mgw_select_option,
          ls_so      TYPE  /iwbep/s_cod_select_option.

    DATA ls_entityset   LIKE LINE OF et_entityset.

    " Taking filters values

    lt_filters = io_tech_request_context->get_filter( )->get_filter_select_options( ).

    " Taking context list filter values

    CLEAR ls_filter.

    READ TABLE lt_filters WITH TABLE KEY property = 'CONTEXTNAMELIST' INTO ls_filter.

    LOOP AT ls_filter-select_options INTO ls_so.

      lv_context_name_list = ls_so-low.

    ENDLOOP.

    " Taking metrics list filter values

    CLEAR ls_filter.

    READ TABLE lt_filters WITH TABLE KEY property = 'METRICNAMELIST' INTO ls_filter.

    LOOP AT ls_filter-select_options INTO ls_so.

      lv_metric_name_list = ls_so-low.

    ENDLOOP.

    " Taking tech scenario filter values

    CLEAR ls_filter.

    READ TABLE lt_filters WITH TABLE KEY property = 'TECHSCENARIO' INTO ls_filter.

    LOOP AT ls_filter-select_options INTO ls_so.

      lv_tech_scenario = ls_so-low.

    ENDLOOP.

    " Default scenario is Systems Monitoring

    IF lv_tech_scenario IS INITIAL.

      lv_tech_scenario = 'T_SYS_MON'.

    ENDIF. " IF lv_tech_scenario IS INITIAL

    " Preparing further filtering on context names (if any)

    IF lv_context_name_list IS NOT INITIAL.

      SPLIT lv_context_name_list AT ';' INTO TABLE lt_context_names.

      LOOP AT lt_context_names ASSIGNING <context_name_line>.

        wa_context_name-sign = 'I'.
        wa_context_name-option = 'CP'.
        wa_context_name-low = <context_name_line>-mo_name.

        APPEND wa_context_name TO lr_context_name.

      ENDLOOP. "LOOP AT lt_context_names ASSIGNING <context_name>

    ENDIF.

    " Preparing further filtering on metrics names (if any)

    IF lv_metric_name_list IS NOT INITIAL.

      SPLIT lv_metric_name_list AT ';' INTO TABLE lt_metric_names.

      LOOP AT lt_metric_names ASSIGNING <metric_name>.

        wa_metric_name-sign = 'I'.
        wa_metric_name-option = 'CP'.
        wa_metric_name-low = <metric_name>-metric_name.

        APPEND wa_metric_name TO lr_metric_name.

      ENDLOOP. "LOOP AT lt_metric_names ASSIGNING <metric_name>

    ENDIF. "  IF lv_metric_name_list IS NOT INITIAL

    " Getting managed objects list according to scenario and context filters

    SELECT context_id context_name FROM v_acentrypoints INTO CORRESPONDING FIELDS OF TABLE lt_mo_selection
      WHERE tech_scenario = lv_tech_scenario
      AND context_name IN lr_context_name
      AND ac_variant = 'A'.

    " Getting list of metrics for selected managed objects according to metrics filtering

    IF lt_mo_selection IS NOT INITIAL.

      SELECT context_id event_type_id name rule_type FROM acmetricdir INTO CORRESPONDING FIELDS OF TABLE lt_acmetricdir
        FOR ALL ENTRIES IN lt_mo_selection
        WHERE context_id  = lt_mo_selection-context_id
        AND name IN lr_metric_name
        AND ac_variant = 'A'
        AND event_class eq cl_alrt_cfg_constants=>ac_entity_metric.

      LOOP AT lt_acmetricdir ASSIGNING <metricdir>.

        READ TABLE lt_mo_selection WITH KEY context_id = <metricdir>-context_id INTO ls_mo_selection.


        ls_entityset-contextid = <metricdir>-context_id.
        ls_entityset-eventtypeid = <metricdir>-event_type_id.
        ls_entityset-mname = <metricdir>-name.
        ls_entityset-contextname = ls_mo_selection-context_name.
        ls_entityset-techscenario = lv_tech_scenario.

        "SELECT SINGLE context_name INTO lv_mo_name FROM v_acentrypoints  WHERE context_id = <metricdir>-context_id.
        "ls_entityset-contextname = lv_mo_name.

        " Getting short text (metric description) for a metric

        SELECT SINGLE short_text INTO lv_short_text FROM acmetricdirt
          WHERE context_id = <metricdir>-context_id
          AND event_type_id = <metricdir>-event_type_id
          AND langu = 'E'.

        ls_entityset-shorttext = lv_short_text.

        " Getting thresholds data

        CASE <metricdir>-rule_type.

          WHEN 'TEXTTHRESHOLD'.
            lv_db_tab_name = 'ACTEXTTHRESHOLD'.
          WHEN 'SIMPLETHRESHOLD'.
            lv_db_tab_name = 'ACSIMPLETHRESDIR'.
          WHEN 'REGEX'.
            lv_db_tab_name = 'ACREGEXDIR'.
          WHEN 'RANGE_THRESHOLD'.
            lv_db_tab_name = 'ACRANGETHRESHDIR'.
          WHEN 'PERCENTAGETHRESHOLD'.
            lv_db_tab_name = 'ACPCTHRESDIR'.
          WHEN 'NUMERIC_GR'.
            lv_db_tab_name = 'ACNUMERIC_G_R'.
          WHEN 'NUMERIC_GYR'.
            lv_db_tab_name = 'ACNUMERIC_GYR'.
          WHEN 'DELTA_THRESHOLD'.
            lv_db_tab_name = 'ACDELTATHRESH'.
          WHEN 'COUNTER_THRESHOLD'.
            lv_db_tab_name = 'ACCOUNTERTHRESH'.
          WHEN 'BEST_OF_LAST_N'.
            lv_db_tab_name = 'ACBESTOFLASTN'.
          WHEN OTHERS.
            CLEAR lv_db_tab_name.
        ENDCASE.

        " For now we will focus only on GREENTOYELLOW and YELLOWTORED threshold types

        IF lv_db_tab_name IS NOT INITIAL.

          " Validation that  greentoyellow field exist in a table     |
          CALL FUNCTION 'DDIF_FIELDINFO_GET'
            EXPORTING
              tabname        = lv_db_tab_name
              fieldname      = 'greentoyellow'
              langu          = sy-langu
            EXCEPTIONS
              not_found      = 1
              internal_error = 2
              OTHERS         = 3.

          IF sy-subrc = 0.

            " Validation that yellowtored field exist in a table
            CALL FUNCTION 'DDIF_FIELDINFO_GET'
              EXPORTING
                tabname        = lv_db_tab_name
                fieldname      = 'yellowtored'
                langu          = sy-langu
              EXCEPTIONS
                not_found      = 1
                internal_error = 2
                OTHERS         = 3.

            IF sy-subrc = 0.

              SELECT SINGLE greentoyellow yellowtored FROM (lv_db_tab_name) INTO (lv_greentoyellow, lv_yellowtored)
                WHERE context_id = <metricdir>-context_id
                AND event_type_id = <metricdir>-event_type_id
                AND ac_variant = 'A'.

              IF sy-subrc = 0.

                ls_entityset-greentoyellow = lv_greentoyellow.
                ls_entityset-yellowtored = lv_yellowtored.

              ENDIF. " IF sy-subrc = 0
            ENDIF. " IF sy-subrc = 0
          ENDIF. " IF sy-subrc = 0
        ENDIF. " IF lv_db_tab_name IS NOT INITIAL

        APPEND ls_entityset TO et_entityset.

      ENDLOOP. " LOOP AT lt_acmetricdir ASSIGNING <metricdir>.

    ENDIF. " IF lt_mo_selection IS NOT INITIAL.

  ENDMETHOD.
ENDCLASS.