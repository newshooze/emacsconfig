(add-to-list 'auto-mode-alist '("\\.csd\\'" . csound-mode))
(add-to-list 'auto-mode-alist '("\\.orc\\'" . csound-mode))
(add-to-list 'auto-mode-alist '("\\.sco\\'" . csound-mode))
(add-to-list 'magic-mode-alist '("<CsoundSynthesi.*>$" . csound-mode))

(setq help-file "~/.vim/doc/csound.txt") ; uses vim style help

(setq csound-builtin '("%" "&" "&&" "+" "-" "/" "<" "<<" "<=" "=" "==" ">" ">=" ">>" "!=")) 
(setq csound-comment '(";")) 
(setq csound-comment-delimiter '("/*" "*/")) 
(setq csound-constant '("odac" "0dbfs" "ctrlinit" "ftgen" "kr" "ksmps" "massign" "nchnls" "pgmassign" "pset" "seed" "sr" "strset")) 
(setq csound-doc '("")) 
(setq csound-keyword '("if" "then" "else" "elseif" "goto" "igoto" "instr" "endin" "opcode" "endop"))
(setq csound-function-name'("ACsoundserver" "AScriptingEnvironment" "ATSSpectralProcessing" "ATSadd" "ATSaddnz" "ATSbufread" "ATScross" "ATSinfo" "ATSinterpread" "ATSpartialtap" "ATSread" "ATSreadnz" "ATSsinnoi" "Appendix A. PitchConversion" "Appendix B. SoundIntensityValues" "Appendix C. FormantValues" "Appendix D. ModalFrequencyRatios" "Appendix E. WindowFunctions" "Appendix F. SoundFont2FileFormat" "Appendix G. CsoundDouble(64-bit)vs.Float(32-bit)" "ArithmeticandLogicOperations" "BasicOscillators" "BuildingCsound" "Command-lineFlags(byCategory)" "CommandLineParameterFile(.csoundrc)" "ComparatorsandAccumulators" "CompilingaCscoreProgram" "ConditionalValues" "Configuring" "ConstantsandVariables" "Converters" "ConvolutionandMorphing" "CopyrightNotice" "Cscore" "CsoundEnvironmentVariables" "CsoundLinks" "CsoundVST" "Csoundcommandline" "Cswish:thewindowingshell" "Delay" "Descriptionofthecommandsyntax" "DirectoriesandFiles" "DurationControlStatements" "DynamicSpectrumOscillators" "EnvelopeGenerators" "EvaluationofExpressions" "EventExtenders" "Example" "Expressions" "ExtendingCsound" "FLTKValuators" "FLTKWidgetsandGUIcontrollers" "FLbox" "FLbutBank" "FLbutton" "FLcloseButton" "FLcolor" "FLcolor2" "FLcount" "FLexecButton" "FLgetsnap" "FLgroup" "FLgroupEnd" "FLhide" "FLhvsBox" "FLhvsBoxSetValue" "FLjoy" "FLkeyIn" "FLknob" "FLlabel" "FLloadsnap" "FLmouse" "FLpack" "FLpackEnd" "FLpack_end" "FLpanel" "FLpanelEnd" "FLpanel_end" "FLprintk" "FLprintk2" "FLroller" "FLrun" "FLsavesnap" "FLscroll" "FLscrollEnd" "FLscroll_end" "FLsetAlign" "FLsetBox" "FLsetColor" "FLsetColor2" "FLsetFont" "FLsetPosition" "FLsetSize" "FLsetSnapGroup" "FLsetText" "FLsetTextColor" "FLsetTextSize" "FLsetTextType" "FLsetVal" "FLsetVal_i" "FLsetsnap" "FLshow" "FLslidBnk" "FLslidBnk2" "FLslidBnk2Set" "FLslidBnk2Setk" "FLslidBnkGetHandle" "FLslidBnkSet" "FLslidBnkSetk" "FLslider" "FLtabs" "FLtabsEnd" "FLtabs_end" "FLtext" "FLupdate" "FLvalue" "FLvkeybd" "FLvslidBnk" "FLvslidBnk2" "FLxyin" "FMSynthesis" "FeaturesofCsoundAC" "FileConversion(HET_IMPORT,HET_EXPORT,PVLOOK,PV_EXPORT,PV_IMPORT," "FileQueries(SNDINFO)" "FrontEnds" "FunctionTableControl" "GEN01" "GEN02" "GEN03" "GEN04" "GEN05" "GEN06" "GEN07" "GEN08" "GEN09" "GEN10" "GEN11" "GEN12" "GEN13" "GEN14" "GEN15" "GEN16" "GEN17" "GEN18" "GEN19" "GEN20" "GEN21" "GEN22" "GEN23" "GEN24" "GEN25" "GEN27" "GEN28" "GEN30" "GEN31" "GEN32" "GEN33" "GEN34" "GEN40" "GEN41" "GEN42" "GEN43" "GEN51" "GEN52" "GENRoutines" "GeneralFLTKWidget-relatedOpcodes" "GenericInputandOutput" "GettingStartedwithCsound" "GranularSynthesis" "HistoryoftheCanonicalCsoundReferenceManual" "HyperVectorialSynthesis" "Imageprocessingopcodes" "IndependentPre-ProcessingwithScsort" "InitializationandReinitialization" "InstrumentControl" "InstrumentInvocation" "InstrumentandOpcodeBlockStatements" "Introduction" "Limitingandwrappingofvectorialcontrolsignals" "LinearAlgebraOpcodes" "LinearPredictiveCoding(LPC)Resynthesis" "LinearandExponentialGenerators" "LorisOpcodes" "MIDI/ScoreInteroperabilityopcodes" "MIDIMessageOutput" "MIDIinput" "Mac" "Macros" "MathematicalFunctions" "MathematicalOperations" "Miscellaneousopcodes" "MixerClear" "MixerGetLevel" "MixerOpcodes" "MixerReceive" "MixerSend" "MixerSetLevel" "ModelsandEmulations" "ModifyingFLTKWidgetAppearance" "MoreAdvancedExamples" "MultipleFileScore" "NamedInstruments" "Network" "Next-PandPrevious-PSymbols" "Nomenclature" "Non-standardSpectralProcessing" "Note-on/Note-offOutput" "OSCandNetwork" "OSCinit" "OSClisten" "OSCsend" "OpcodeEquivalentsofFunctions" "OpcodeQuickReference" "OperationsBetweenaVectorialandaScalarSignal" "OperationsBetweentwoVectorialSignals" "OptimizingAudioI/OLatency" "OrchestraOpcodesandOperators" "OrchestraSyntax" "OrdinaryStatements" "OtherCsoundUtilities(CS,CSB64ENC,ENVEXT,EXTRACTOR,MAKECSD,MIXER," "OtherFLTKWidgets" "PanningandSpatialization" "Part I. Overview" "Part II. OpcodesOverview" "Part III. Reference" "Part IV. OpcodeQuickReference" "Phasors" "PitchConverters" "PluginHosting" "Preface" "Prev   " "PrintingandDisplay" "ProgramFlowControl" "PythonOpcodes" "Ramping" "Random(Noise)Generators" "RandomFunctions" "Read/WriteOperations" "Real-TimeAudio" "Real-timeMIDISupport" "Real-timePerformanceControl" "RecentDevelopments" "RemoteOpcodes" "Reverberation" "SampleLevelOperators" "SamplePlayback" "ScannedSynthesis" "ScoreFilePreprocessing" "ScoreMacros" "ScoreStatements" "ScoreStatementsandGENRoutines" "SensingandControl" "SignalGenerators" "SignalInput" "SignalInputandOutput" "SignalLimiters" "SignalModifiers" "SignalOutput" "SliderBanks" "SoftwareBus" "SoundFileQueries" "SoundfileFormats." "SpecialEffects" "SpecializedFilters" "SpectralProcessing" "Stacks" "StandardFilters" "StringConversionOpcodes" "Strings" "Stringsinp-fields" "Sub-instrumentControl" "SyntaxoftheOrchestra" "SystemRealtimeMessages" "TableAccess" "TableReadingwithDynamicSelection" "TclCsound" "TclCsoundCommandReference" "TclCsoundasalanguagewrapper" "TheCsoundCommand" "TheStandardNumericScore" "TheUtilityPrograms" "TimeReading" "ToolsforReal-timeSpectralProcessing(pvsopcodes)" "TrigonometricFunctions" "TuningOpcodes" "UnifiedFileFormatforOrchestrasandScores" "UserDefinedOpcodes(UDO)" "UsingCsound" "VSTforCsound" "VectorialControl-rateDelayPaths" "VectorialEnvelopeGenerators" "VectorialOpcodes" "VectorialRandomSignalGenerators" "WaveTerrainSynthesis" "WaveguidePhysicalModeling" "Waveguides" "WaveshapingandPhaseDistortion" "What'snewinCsound5.10" "Windows" "WritingaCscoreControlProgram" "ZakPatchSystem" "^" "a" "aStatement(orAdvanceStatement)" "abetarand" "abexprnd" "abs" "acauchy" "active" "adsr" "adsyn" "adsynt" "adsynt2" "aexprand" "aftouch" "agauss" "agogobel" "alinrand" "alpass" "ampdb" "ampdbfs" "ampmidi" "any" "apcauchy" "apoisson" "apow" "areson" "aresonk" "atone" "atonek" "atonex" "atrirand" "atsa" "aunirand" "aweibull" "bStatement" "babo" "balance" "bamboo" "barmodel" "bbcutm" "bbcuts" "betarand" "bexprnd" "bformdec" "bformdec1" "bformenc" "bformenc1" "binit" "biquad" "biquada" "birnd" "bqrez" "butbp" "butbr" "buthp" "butlp" "butterbp" "butterbr" "butterhp" "butterlp" "button" "buzz" "cabasa" "cauchy" "ceil" "cent" "cggoto" "chanctrl" "changed" "chani" "chano" "chebyshevpoly" "checkbox" "chn" "chnclear" "chnexport" "chnget" "chnmix" "chnparams" "chnset" "chuap" "cigoto" "ckgoto" "clear" "clfilt" "clip" "clock" "clockoff" "clockon" "cngoto" "comb" "compress" "control" "convle" "convolve" "cos" "cosh" "cosinv" "cps2pch" "cpsmidi" "cpsmidib" "cpsmidinn" "cpsoct" "cpspch" "cpstmid" "cpstun" "cpstuni" "cpsxpch" "cpuprc" "cross2" "crunch" "cs" "csb64enc" "ctrl14" "ctrl21" "ctrl7" "ctrlinit" "cuserrnd" "cvanal" "dam" "date" "dates" "db" "dbamp" "dbfsamp" "dcblock" "dcblock2" "dconv" "delay" "delay1" "delayk" "delayr" "delayw" "deltap" "deltap3" "deltapi" "deltapn" "deltapx" "deltapxw" "denorm" "diff" "diskgrain" "diskin" "diskin2" "dispfft" "display" "distort" "distort1" "divz" "dnoise" "downsamp" "dripwater" "dssiactivate" "dssiaudio" "dssictls" "dssiinit" "dssilist" "dumpk" "dumpk2" "dumpk3" "dumpk4" "duserrnd" "eStatement" "envext" "envlpx" "envlpxr" "ephasor" "eqfil" "event" "event_i" "exitnow" "exp" "expcurve" "expon" "exprand" "expseg" "expsega" "expsegr" "extractor" "fStatement(orFunctionTableStatement)" "ficlose" "filelen" "filenchnls" "filepeak" "filesr" "filter2" "fin" "fini" "fink" "fiopen" "flanger" "flashtxt" "flooper" "flooper2" "floor" "fluidAllOut" "fluidCCi" "fluidCCk" "fluidControl" "fluidEngine" "fluidLoad" "fluidNote" "fluidOut" "fluidProgramSelect" "fluidSetInterpMethod" "fmb3" "fmbell" "fmmetal" "fmpercfl" "fmrhode" "fmvoice" "fmwurlie" "fof" "fof2" "fofilter" "fog" "fold" "follow" "follow2" "foscil" "foscili" "fout" "fouti" "foutir" "foutk" "fprintks" "fprints" "frac" "freeverb" "ftchnls" "ftconv" "ftfree" "ftgen" "ftgentmp" "ftlen" "ftload" "ftloadk" "ftlptim" "ftmorf" "ftsave" "ftsavek" "ftsr" "gain" "gainslider" "gauss" "gbuzz" "getcfg" "gogobel" "goto" "grain" "grain2" "grain3" "granule" "guiro" "harmon" "harmon2" "het_export" "het_import" "hetro" "hilbert" "hrtfer" "hrtfmove" "hrtfmove2" "hrtfstat" "hsboscil" "hvs1" "hvs2" "hvs3" "i" "iStatement(InstrumentorNoteStatement)" "ibetarand" "ibexprnd" "icauchy" "ictrl14" "ictrl21" "ictrl7" "iexprand" "igauss" "igoto" "ihold" "ilinrand" "imagecreate" "imagefree" "imagegetpixel" "imageload" "imagesave" "imagesetpixel" "imagesize" "imidic14" "imidic21" "imidic7" "in" "in32" "inch" "inh" "init" "initc14" "initc21" "initc7" "ino" "inq" "inrg" "ins" "insglobal" "insremot" "instimek" "instimes" "int" "integ" "interp" "invalue" "inx" "inz" "ioff" "ion" "iondur" "iondur2" "ioutat" "ioutc" "ioutc14" "ioutpat" "ioutpb" "ioutpc" "ipcauchy" "ipoisson" "ipow" "is16b14" "is32b14" "islider16" "islider32" "islider64" "islider8" "itablecopy" "itablegpw" "itablemix" "itablew" "itrirand" "iunirand" "iweibull" "jacktransport" "jitter" "jitter2" "jspline" "k" "kbetarand" "kbexprnd" "kcauchy" "kdump" "kdump2" "kdump3" "kdump4" "kexprand" "kfilter2" "kgauss" "kgoto" "klinrand" "kon" "koutat" "koutc" "koutc14" "koutpat" "koutpb" "koutpc" "kpcauchy" "kpoisson" "kpow" "kr" "kread" "kread2" "kread3" "kread4" "ksmps" "ktableseg" "ktrirand" "kunirand" "kweibull" "lfo" "limit" "line" "linen" "linenr" "lineto" "linrand" "linseg" "linsegr" "locsend" "locsig" "log" "log10" "logbtwo" "logcurve" "loop_ge" "loop_gt" "loop_le" "loop_lt" "loopseg" "loopsegp" "lorenz" "lorismorph" "lorisplay" "lorisread" "loscil" "loscil3" "loscilx" "lowpass2" "lowres" "lowresx" "lpanal" "lpf18" "lpfreson" "lphasor" "lpinterp" "lposcil" "lposcil3" "lposcila" "lposcilsa" "lposcilsa2" "lpread" "lpreson" "lpshold" "lpsholdp" "lpslot" "mStatement(MarkStatement)" "mac" "maca" "madsr" "makecsd" "mandel" "mandol" "marimba" "massign" "max" "max_k" "maxabs" "maxabsaccum" "maxaccum" "maxalloc" "mclock" "mdelay" "metro" "midglobal" "midic14" "midic21" "midic7" "midichannelaftertouch" "midichn" "midicontrolchange" "midictrl" "mididefault" "midiin" "midinoteoff" "midinoteoncps" "midinoteonkey" "midinoteonoct" "midinoteonpch" "midion" "midion2" "midiout" "midipitchbend" "midipolyaftertouch" "midiprogramchange" "miditempo" "midremot" "min" "minabs" "minabsaccum" "minaccum" "mirror" "mixer" "mode" "monitor" "moog" "moogladder" "moogvcf" "moogvcf2" "moscil" "mpulse" "mrtmsg" "multitap" "must" "mute" "mxadsr" "nStatement" "nchnls" "nestedap" "nlfilt" "noise" "noteoff" "noteon" "noteondur" "noteondur2" "notnum" "nreverb" "nrpn" "nsamp" "nstrnum" "ntrpol" "octave" "octcps" "octmidi" "octmidib" "octmidinn" "octpch" "oscbnk" "oscil" "oscil1" "oscil1i" "oscil3" "oscili" "oscilikt" "osciliktp" "oscilikts" "osciln" "oscils" "oscilx" "out" "out32" "outc" "outch" "outh" "outiat" "outic" "outic14" "outipat" "outipb" "outipc" "outkat" "outkc" "outkc14" "outkpat" "outkpb" "outkpc" "outo" "outq" "outq1" "outq2" "outq3" "outq4" "outrg" "outs" "outs1" "outs2" "outvalue" "outx" "outz" "p" "pan" "pan2" "pareq" "partials" "partikkel" "partikkelsync" "pcauchy" "pchbend" "pchmidi" "pchmidib" "pchmidinn" "pchoct" "pconvolve" "pcount" "pdclip" "pdhalf" "pdhalfy" "peak" "peakk" "pgmassign" "phaser1" "phaser2" "phasor" "phasorbnk" "pindex" "pinkish" "pitch" "pitchamdf" "planet" "pluck" "poisson" "polyaft" "polynomial" "pop" "pop_f" "port" "portk" "poscil" "poscil3" "pow" "powershape" "powoftwo" "prealloc" "prepiano" "print" "printf" "printk" "printk2" "printks" "prints" "product" "pset" "ptrack" "push" "push_f" "puts" "pv_export" "pv_import" "pvadd" "pvanal" "pvbufread" "pvcross" "pvinterp" "pvlook" "pvoc" "pvread" "pvsadsyn" "pvsanal" "pvsarp" "pvsbandp" "pvsbandr" "pvsbin" "pvsblur" "pvsbuffer" "pvsbufread" "pvscale" "pvscent" "pvscross" "pvsdemix" "pvsdiskin" "pvsdisp" "pvsfilter" "pvsfread" "pvsfreeze" "pvsftr" "pvsftw" "pvsfwrite" "pvshift" "pvsifd" "pvsin" "pvsinfo" "pvsinit" "pvsmaska" "pvsmix" "pvsmooth" "pvsmorph" "pvsosc" "pvsout" "pvspitch" "pvstencil" "pvsvoc" "pvsynth" "pycallOpcodes" "pyexecOpcodes" "pyrunOpcodes" "qStatement" "rStatement(RepeatStatement)" "rand" "randh" "randi" "random" "randomh" "randomi" "rbjeq" "readclock" "readk" "readk2" "readk3" "readk4" "reinit" "release" "remoteport" "remove" "repluck" "reson" "resonk" "resonr" "resonx" "resonxk" "resony" "resonz" "resyn" "reverb" "reverb2" "reverbsc" "rewindscore" "rezzy" "rigoto" "rireturn" "rms" "rnd" "rnd31" "round" "rspline" "rtclock" "s16b14" "s32b14" "sStatement" "samphold" "sandpaper" "scale" "scanhammer" "scans" "scantable" "scanu" "schedkwhen" "schedkwhennamed" "schedule" "schedwhen" "scoreline" "scoreline_i" "sdif2ad" "seed" "sekere" "semitone" "sense" "sensekey" "seqtime" "seqtime2" "setctrl" "setksmps" "setscorepos" "sfilist" "sfinstr" "sfinstr3" "sfinstr3m" "sfinstrm" "sfload" "sflooper" "sfpassign" "sfplay" "sfplay3" "sfplay3m" "sfplaym" "sfplist" "sfpreset" "shaker" "sin" "sinh" "sininv" "sinsyn" "sleighbells" "slider16" "slider16f" "slider16table" "slider16tablef" "slider32" "slider32f" "slider32table" "slider32tablef" "slider64" "slider64f" "slider64table" "slider64tablef" "slider8" "slider8f" "slider8table" "slider8tablef" "sliderKawai" "sndinfo" "sndload" "sndloop" "sndwarp" "sndwarpst" "sockrecv" "socksend" "soundin" "soundout" "soundouts" "space" "spat3d" "spat3di" "spat3dt" "spdist" "specaddm" "specdiff" "specdisp" "specfilt" "spechist" "specptrk" "specscal" "specsum" "spectrum" "splitrig" "sprintf" "sprintfk" "spsend" "sqrt" "sr" "srconv" "stack" "statevar" "stix" "strcat" "strcatk" "strchar" "strchark" "strcmp" "strcmpk" "strcpy" "strcpyk" "streson" "strget" "strindex" "strindexk" "strlen" "strlenk" "strlower" "strlowerk" "strrindex" "strrindexk" "strset" "strsub" "strsubk" "strtod" "strtodk" "strtol" "strtolk" "strupper" "strupperk" "subinstr" "subinstrinit" "sum" "svfilter" "syncgrain" "syncloop" "syncphasor" "system" "tStatement(TempoStatement)" "tab" "table" "table3" "tablecopy" "tablegpw" "tablei" "tableicopy" "tableigpw" "tableikt" "tableimix" "tableiw" "tablekt" "tablemix" "tableng" "tablera" "tableseg" "tablew" "tablewa" "tablewkt" "tablexkt" "tablexseg" "tabmorph" "tabmorpha" "tabmorphak" "tabmorphi" "tabplay" "tabrec" "tambourine" "tan" "tanh" "taninv" "taninv2" "tb" "tbvcf" "tempest" "tempo" "tempoval" "tigoto" "timedseq" "timeinstk" "timeinsts" "timek" "times" "timout" "tival" "tlineto" "tone" "tonek" "tonex" "tradsyn" "trandom" "transeg" "trcross" "trfilter" "trhighest" "trigger" "trigseq" "trirand" "trlowest" "trmix" "trscale" "trshift" "trsplit" "turnoff" "turnoff2" "turnon" "unirand" "upsamp" "urd" "vStatement" "vadd" "vadd_i" "vaddv" "vaddv_i" "vaget" "valpass" "vaset" "vbap16" "vbap16move" "vbap4" "vbap4move" "vbap8" "vbap8move" "vbaplsinit" "vbapz" "vbapzmove" "vcella" "vco" "vco2" "vco2ft" "vco2ift" "vco2init" "vcomb" "vcopy" "vcopy_i" "vdelay" "vdelay3" "vdelayk" "vdelayx" "vdelayxq" "vdelayxs" "vdelayxw" "vdelayxwq" "vdelayxws" "vdivv" "vdivv_i" "vecdelay" "veloc" "vexp" "vexp_i" "vexpseg" "vexpv" "vexpv_i" "vibes" "vibr" "vibrato" "vincr" "vlimit" "vlinseg" "vlowres" "vmap" "vmirror" "vmult" "vmult_i" "vmultv" "vmultv_i" "voice" "vosim" "vphaseseg" "vport" "vpow" "vpow_i" "vpowv" "vpowv_i" "vpvoc" "vrandh" "vrandi" "vstaudio,vstaudiog" "vstbankload" "vstedit" "vstinfo" "vstinit" "vstmidiout" "vstnote" "vstparamset,vstparamget" "vstprogset" "vsubv" "vsubv_i" "vtaba" "vtabi" "vtabk" "vtable1k" "vtablea" "vtablei" "vtablek" "vtablewa" "vtablewi" "vtablewk" "vtabwa" "vtabwi" "vtabwk" "vwrap" "waveset" "weibull" "wgbow" "wgbowedbar" "wgbrass" "wgclar" "wgflute" "wgpluck" "wgpluck2" "wguide1" "wguide2" "wrap" "wterrain" "xStatement" "xadsr" "xin" "xout" "xscanmap" "xscans" "xscansmap" "xscanu" "xtratim" "xyin" "zacl" "zakinit" "zamod" "zar" "zarg" "zaw" "zawm" "zfilter2" "zir" "ziw" "ziwm" "zkcl" "zkmod" "zkr" "zkw" "zkwm"))
(setq csound-negation-char '(""))
(setq csound-preprocessor '("#define" "#ifdef" "#ifndef" "#include" "#undef"))
(setq csound-type '("<" ">" "CsoundSynthesizer" "CsInstruments" "CsOptions" "CsScore")) 
(setq csound-string '("\"*\""))
(setq csound-variable-name '("")) 
(setq csound-warning '(""))

(setq csound-builtin-re (regexp-opt csound-builtin 'words))
(setq csound-comment-delimiter-re (regexp-opt csound-comment-delimiter 'words)) 
(setq csound-comment-re (regexp-opt csound-comment 'words)) 
(setq csound-constant-re (regexp-opt csound-constant 'words))	
(setq csound-doc-re (regexp-opt csound-doc 'words)) 
(setq csound-keyword-re (regexp-opt csound-keyword 'words)) 
(setq csound-function-name-re (regexp-opt csound-function-name 'words))
(setq csound-negation-char-re (regexp-opt csound-negation-char 'words)) 
(setq csound-preprocessor-re (regexp-opt csound-preprocessor 'words))
(setq csound-string-re (regexp-opt csound-string 'words))
(setq csound-type-re (regexp-opt csound-type 'words))
(setq csound-variable-name-re (regexp-opt csound-variable-name 'words)) 
(setq csound-warning-re (regexp-opt csound-warning 'words))

(setq csound-font-lock-keywords
	`( 
	 (,csound-builtin-re . font-lock-builtin-face)
	 (,csound-comment-delimiter-re . font-lock-comment-delimiter-face)
	 (,csound-comment-re . font-lock-comment-face)
	 (,csound-constant-re . font-lock-constant-face)
	 (,csound-doc-re . font-lock-doc-face)
	 (,csound-keyword-re . font-lock-keyword-face)
	 (,csound-function-name-re . font-lock-function-name-face)
	 (,csound-negation-char-re . font-lock-negation-char-face)
	 (,csound-preprocessor-re . font-lock-preprocessor-face)
	 (,csound-string-re . font-lock-string-face)
	 (,csound-type-re . font-lock-type-face)
	 (,csound-variable-name-re . font-lock-variable-face)
	 (,csound-warning-re . font-lock-warning-face)))

(defun csound-create-syntax-table ()
  (interactive)
  (setq csound-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\; "< b" csound-mode-syntax-table)
  (modify-syntax-entry ?\n "> b" csound-mode-syntax-table)
  (modify-syntax-entry ?\" "\"" csound-mode-syntax-table)
  (modify-syntax-entry ?\/ ". 14" csound-mode-syntax-table)
  (modify-syntax-entry ?* ". 23" csound-mode-syntax-table))

(csound-create-syntax-table)

(define-derived-mode csound-mode fundamental-mode
  :syntax-table csound-mode-syntax-table
  (setq mode-name "csound")
  (setq font-lock-defaults '(csound-font-lock-keywords))
	(setq csound-builtin-re nil)
	(setq csound-comment-delimiter-re nil)
	(setq csound-comment-re nil)
	(setq csound-constant-re nil)
	(setq csound-doc-re nil)
	(setq csound-keyword-re nil)
	(setq csound-function-name-re nil)
	(setq csound-negation-char-re nil)
	(setq csound-preprocessor-re nil)
	(setq csound-string-re nil)
	(setq csound-syntactic-re nil)
	(setq csound-type-re nil)
	(setq csound-warning-re nil))

(defun csound-play ()
	"csound-play-score Executes the compile function (compile \"csound \" (buffer-file-name))"
  (interactive)
  (save-buffer)
  (setq command-line (concat "csound " (buffer-file-name)))
  (compile command-line))

(defun csound-compilation-finish-function (buffer exitcode)
  "Kill compilation window when compiling is done "
  (interactive)
  (delete-window (get-buffer-window (get-buffer "*compilation*")))
  (kill-buffer "*compilation*")
  (cons exitcode buffer))

(defun csound-mode-keys ()
  (local-set-key (kbd "<f5>") 'csound-play)
  (local-set-key (kbd "<S-f5>") 'kill-compilation)
	; evil-mode maps Shift + k to evil-lookup-func (man)
	; If running in evil-mode intercept evil-lookup-function
	; Else map Shift + k
	(if (evil-mode) 
		(setq-local evil-lookup-func 'csound-lookup-func)
		(loacl-set-key (kbd "S-k") 'csound-lookup-func)))

(defun csound-lookup-func ()
	(setq word-at-cursor(thing-at-point 'word))
	(progn 
		(split-window-horizontally -80)
		(setq search-string (concat "*" word-at-cursor "*"))
		(find-file-other-window help-file)
		(beginning-of-buffer)
		(search-forward search-string)
		(recenter-top-bottom 0) ; bring the help topic to the top of the window
		(beginning-of-line)
		(vimhelp-mode)))

(setq compilation-window-height 6)

(add-hook 'compilation-finish-functions 'csound-compilation-finish-function t t)
(add-hook 'csound-mode-hook 'csound-mode-keys)

(provide 'csound-mode)
