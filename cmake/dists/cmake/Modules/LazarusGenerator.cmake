# Copyright (c) 2009 Tobias Gunkel <tobigun@at@users.sourceforge.net>
#
# Copying and distribution of this file, with or without
# modification, are permitted in any medium without royalty provided
# the copyright notice and this notice are preserved.

function(lazarus_add_unit_entry list index src name)
  set(indent "      ")
  set(tmplist "${tmplist}\n${indent}<Unit${index}>\n")
  set(tmplist "${tmplist}${indent}  <Filename Value=\"${src}\"/>\n")
  set(tmplist "${tmplist}${indent}  <IsPartOfProject Value=\"True\"/>\n")
  if(name)
  set(tmplist "${tmplist}${indent}  <UnitName Value=\"${name}\"/>\n")
  endif(name)
  set(tmplist "${tmplist}${indent}</Unit${index}>")
  set(${list} "${${list}}${tmplist}" PARENT_SCOPE)
endfunction(lazarus_add_unit_entry)

function(GENERATE_LAZARUS_PROJECT
  proj_src 
  output_name 
  unit_out_dir
  unit_list
  unit_dirs
  include_list
  include_dirs
  object_dirs
  pflags
)
  set(LAZARUS_TGT_FILE_EXT ${CMAKE_EXECUTABLE_SUFFIX})
  set(LAZARUS_PATH_DELIM "/")

  # set project name
  get_filename_component(proj_name ${proj_src} NAME_WE)
  set(LAZARUS_PROJECT_TITLE "${proj_name}")

  # set output name (without extension)
  set(LAZARUS_OUTPUT_NAME ${output_name})

  # collect project file names (sources + includes)
  set(proj_files "")
  foreach(item ${unit_list} ${include_list})
    file(GLOB files "${item}")
    list(APPEND proj_files ${files}) 
  endforeach(item)
    
  # add project source file as first entry to unit list
  lazarus_add_unit_entry(LAZARUS_UNIT_LIST 0 ${proj_src} "${proj_name}")

  # add additional source/include files to unit list
  set(proj_file_index 1)
  foreach(file ${proj_files})
    lazarus_add_unit_entry(LAZARUS_UNIT_LIST ${proj_file_index} ${file} "")
    math(EXPR proj_file_index "${proj_file_index}+1")
  endforeach(file)
  set(LAZARUS_UNITS_COUNT ${proj_file_index})

  # set paths
  set(LAZARUS_UNIT_OUTPUT_DIRECTORY ${unit_out_dir})
  set(LAZARUS_INCLUDE_PATH "${include_dirs}")
  set(LAZARUS_SRC_PATH "${unit_dirs}")

  # init custom flags
  set(custom_flags_list "")

  # add PFLAGS
  list(APPEND custom_flags_list ${pflags})

  # add object paths to custom flags
  foreach(obj_dir ${obj_dirs})
    #get_filename_component(obj_abs_dir ${obj_dir} ABSOLUTE)
    #file(RELATIVE_PATH obj_rel_dir ${work_dir} ${flag_abs_dir})
    list(APPEND custom_flags_list -Fo${obj_dir})
  endforeach(obj_dir)

  # convert custom flag list into a space separated string
  set(LAZARUS_CUSTOM_OPTIONS)
  foreach(flag ${custom_flags_list})
    set(LAZARUS_CUSTOM_OPTIONS "${LAZARUS_CUSTOM_OPTIONS} ${flag}")    
  endforeach(flag)

  # set flags
  set(LAZARUS_DEBUG        False)
  set(LAZARUS_ALWAYS_BUILD False)

  set(LAZARUS_VERBOSITY)
  #  <Other>
  #    <Verbosity>
  #      <ShowErrors Value="False"/>
  #      <ShowWarn Value="False"/>
  #      <ShowNotes Value="False"/>
  #      <ShowHints Value="False"/>
  #      <ShowGenInfo Value="False"/>
  #      <ShoLineNum Value="True"/>
  #      <ShowAll Value="True"/>
  #      <ShowAllProcsOnError Value="True"/>
  #      <ShowDebugInfo Value="True"/>
  #      <ShowUsedFiles Value="True"/>
  #      <ShowTriedFiles Value="True"/>
  #      <ShowDefMacros Value="True"/>
  #      <ShowCompProc Value="True"/>
  #      <ShowCond Value="True"/>
  #      <ShowExecInfo Value="True"/>
  #      <ShowNothing Value="True"/>
  #      <ShowSummary Value="True"/>
  #      <ShowHintsForUnusedUnitsInMainSrc Value="True"/>
  #      <ShowHintsForSenderNotUsed Value="True"/>
  #    </Verbosity>
  #    <WriteFPCLogo Value="False"/>
  #    <ConfigFile>
  #      <DontUseConfigFile Value="True"/>
  #    </ConfigFile>
  #    <CustomOptions Value="-lo"/>
  #  </Other>

  # create project file
  get_filename_component(current_list_dir ${CMAKE_CURRENT_LIST_FILE} PATH)
  configure_file(${current_list_dir}/Templates/lazarusProj.lpi.in
                 ${CMAKE_CURRENT_SOURCE_DIR}/${target}.lpi
                 @ONLY)

endfunction(GENERATE_LAZARUS_PROJECT)


#  <CompilerOptions>
#    <SearchPaths>
#      <IncludeFiles Value="includes/"/>
#      <Libraries Value="libs/"/>
#      <UnitOutputDirectory Value="units-out"/>
#      <SrcPath Value="other/"/>
#    </SearchPaths>
#    <Parsing>
#      <SyntaxOptions>
#        <SyntaxMode Value="Delphi"/>
#        <IncludeAssertionCode Value="True"/>
#        <AllowLabel Value="False"/>
#        <CPPInline Value="False"/>
#        <CStyleMacros Value="True"/>
#        <InitConstructor Value="True"/>
#        <StaticKeyword Value="True"/>
#        <UseAnsiStrings Value="True"/>
#      </SyntaxOptions>
#    </Parsing>
#    <CodeGeneration>
#      <SmartLinkUnit Value="True"/>
#      <Checks>
#        <IOChecks Value="True"/>
#        <RangeChecks Value="True"/>
#        <OverflowChecks Value="True"/>
#        <StackChecks Value="True"/>
#      </Checks>
#      <VerifyObjMethodCallValidity Value="True"/>
#      <Optimizations>
#        <OptimizationLevel Value="0"/>
#      </Optimizations>
#    </CodeGeneration>
#    <Linking>
#      <Debugging>
#        <GenerateDebugInfo Value="True"/>
#        <UseLineInfoUnit Value="False"/>
#        <UseHeaptrc Value="True"/>
#        <UseValgrind Value="True"/>
#        <GenGProfCode Value="True"/>
#        <StripSymbols Value="True"/>
#      </Debugging>
#      <LinkSmart Value="True"/>
#      <Options>
#        <PassLinkerOptions Value="True"/>
#        <LinkerOptions Value="hallo"/>
#      </Options>
#    </Linking>
#    <Other>
#      <Verbosity>
#        <ShowErrors Value="False"/>
#        <ShowWarn Value="False"/>
#        <ShowNotes Value="False"/>
#        <ShowHints Value="False"/>
#        <ShowGenInfo Value="False"/>
#        <ShoLineNum Value="True"/>
#        <ShowAll Value="True"/>
#        <ShowAllProcsOnError Value="True"/>
#        <ShowDebugInfo Value="True"/>
#        <ShowUsedFiles Value="True"/>
#        <ShowTriedFiles Value="True"/>
#        <ShowDefMacros Value="True"/>
#        <ShowCompProc Value="True"/>
#        <ShowCond Value="True"/>
#        <ShowExecInfo Value="True"/>
#        <ShowNothing Value="True"/>
#        <ShowSummary Value="True"/>
#        <ShowHintsForUnusedUnitsInMainSrc Value="True"/>
#        <ShowHintsForSenderNotUsed Value="True"/>
#      </Verbosity>
#      <WriteFPCLogo Value="False"/>
#      <ConfigFile>
#        <DontUseConfigFile Value="True"/>
#      </ConfigFile>
#    </Other>
#  </CompilerOptions>
