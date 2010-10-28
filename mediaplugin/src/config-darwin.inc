{***************************************************************** 
 * Configuration file for ultrastardx 1.1
 * src/config-darwin.inc.  Generated from config.inc.in by configure.
 *****************************************************************}

{* Libraries *}

{$IF Defined(IncludeConstants)}
  lua_lib_name = 'lua';
{$IFEND}

{$UNDEF HaveProjectM}
{$IF Defined(HaveProjectM) and Defined(IncludeConstants)}
  ProjectM_DataDir = '';
  PROJECTM_VERSION_MAJOR   = 0;
  PROJECTM_VERSION_MINOR   = 0;
  PROJECTM_VERSION_RELEASE = 0;
{$IFEND}

{$DEFINE HavePortaudio}
{$IF Defined(HavePortaudio) and Defined(IncludeConstants)}
  PORTAUDIO_VERSION_MAJOR   = 19;
  PORTAUDIO_VERSION_MINOR   = 0;
  PORTAUDIO_VERSION_RELEASE = 0;
{$IFEND}

{$DEFINE HaveLibPcre}
{$IF Defined(HaveLibPcre) and Defined(IncludeConstants)}
  LIBPCRE_LIBDIR   = '/sw/lib';
{$IFEND}

{$UNDEF HavePortmixer}
