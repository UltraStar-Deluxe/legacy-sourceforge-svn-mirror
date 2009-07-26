# otool: Mac OS X object file displaying tool
find_program(OTOOL otool) 
# install_name_tool: Mac OS X tool to change dynamic shared library install names
find_program(INSTALL_NAME_TOOL install_name_tool)
# hdiutil: Mac OS X disk image tool
find_program(HDIUTIL hdiutil)

set(macosx_bundle_path "${PROJECT_BINARY_DIR}/UltraStarDeluxe.app/Contents")

add_custom_target(macosx-switch-installdir
  COMMAND mkdir -p "${macosx_bundle_path}"
  COMMAND ${CMAKE_COMMAND}
    -DCMAKE_INSTALL_DIR=${PROJECT_BINARY_DIR}
    -DBINDIR=${macosx_bundle_path}/MacOS
    -DDATADIR=${macosx_bundle_path}
    ${PROJECT_SOURCE_DIR}
)

add_custom_target(macosx-app
  COMMENT "Creating Mac OS X application"

  COMMAND mkdir -p "${macosx_bundle_path}/Resources"

  # copy the icon (MUST be done BEFORE info.plist is created)
  COMMAND install "${PROJECT_SOURCE_DIR}/icons/ultrastardx.icns" "${macosx_bundle_path}/Resources/"
  COMMAND install "${PROJECT_SOURCE_DIR}/src/macosx/Info.plist" "${macosx_bundle_path}/"

  # copy the resources
  COMMAND ${CMAKE_BUILD_TOOL} install

  # create the song directory
  COMMAND mkdir -p "${macosx_bundle_path}/songs"
)
add_dependencies(macosx-app macosx-switch-installdir all)


#define install_osx_libraries
#    # copy the dylib and change its install names
#    $(shell $(INSTALL) -m 755 $(dylib) $(macosx_bundle_path)/MacOS)
#    $(shell $(INSTALL_NAME_TOOL) -change $(dylib) @executable_path/$(notdir $(dylib)) $(macosx_bundle_path)/MacOS/ultrastardx)
#    $(shell $(INSTALL_NAME_TOOL) -id @executable_path/$(notdir $(dylib)) $(macosx_bundle_path)/MacOS/$(notdir $(dylib)))
#    $(foreach linked_dylibs_2,$(shell $(OTOOL) -L $(dylib) | grep version | cut -f 1 -d ' ' | grep -v \/System\/Library | grep -v \usr\/lib | grep -v executable_path),$(rename_secondary_osx_libraries))
#endef
#
#define rename_secondary_osx_libraries
#    $(shell $(INSTALL_NAME_TOOL) -change $(linked_dylibs_2) @executable_path/$(notdir $(linked_dylibs_2)) $(macosx_bundle_path)/MacOS/$(notdir $(dylib)))
#endef
#
#$(foreach dylib,$(shell $(OTOOL) -L $(macosx_bundle_path)/MacOS/ultrastardx | grep version | cut -f 1 -d ' ' | grep -v \/System\/Library | grep -v \/usr\/lib),$(install_osx_libraries))
#$(foreach dylib,$(shell $(OTOOL) -L /sw/lib/libavcodec.dylib   | grep version | cut -f 1 -d ' ' | grep -v \/System\/Library | grep -v \/usr\/lib),$(install_osx_libraries))


# Create double clickable standalone (does not need fink) Mac OS X 
# application. Not fully test, but should work on 10.5.
add_custom_target(macosx-standalone-app
  COMMENT "Creating standalone Mac OS X application"

  # install fink libs

  include(AnalyzeDylibDeps)
  analyze_dylib_deps(${usdx_exe} libdeps)
  set(USDX_DYLIB_DIR ${CMAKE_CURRENT_BINARY_DIR}/dylib)

  # install() does not follow symlinks so we have to copy the libs at cmake time
  file(MAKE_DIRECTORY ${USDX_DYLIB_DIR})
  foreach(dylib ${libdeps})
    get_filename_component(dylib_name ${dylib} NAME)
    file_copy_if_different(${dylib} ${USDX_DYLIB_DIR}/${dylib_name})
  endforeach()

  install(DIRECTORY ${USDX_DYLIB_DIR}/ DESTINATION dylib USE_SOURCE_PERMISSIONS)
)
add_dependencies(macosx-standalone-app macosx-app)

add_custom_target(macosx-dmg
  COMMENT "Creating Mac OS X DMG-image"

  COMMAND rm UltraStarDeluxe.dmg
  COMMAND ${HDIUTIL} create -type SPARSE -size 100m -fs HFS+ -volname UltraStarDeluxe -ov -attach UltraStarDeluxe.sparseimage
  COMMAND cp -R UltraStarDeluxe.app /Volumes/UltraStarDeluxe
  #cp ultrastardx/icons/UltraStarDeluxeVolumeIcon.icns /Volumes/UltraStarDeluxe/.VolumeIcon.icns
  #/Developer/Tools/SetFile -a C /Volumes/UltraStarDeluxe/.VolumeIcon.icns /Volumes/UltraStarDeluxe
  COMMAND ${HDIUTIL} detach /Volumes/UltraStarDeluxe
  COMMAND ${HDIUTIL} convert UltraStarDeluxe.sparseimage -format UDBZ -o UltraStarDeluxe.dmg
  COMMAND rm UltraStarDeluxe.sparseimage
)
add_dependencies(macosx-dmg macosx-standalone-app)

# remove Mac OS X apllication bundle and disk image
add_custom_target(clean-macosx)
add_dependencies(clean-macosx clean-macosx-app clean-macosx-dmg)

add_custom_target(clean-macosx-app
  COMMAND rm -r UltraStarDeluxe.app 
)

add_custom_target(clean-macosx-dmg
  COMMAND rm UltraStarDeluxe.dmg 
)
