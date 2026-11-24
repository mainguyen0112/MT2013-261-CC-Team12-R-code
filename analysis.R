# MT2013 - Team 12: Internet Advertisement Analysis
# Statistical summaries, inference, Logistic Regression, Random Forest and XGBoost.
#
# Input: put cleaned_data.csv (submitted separately) beside this file.
# Terminal: Rscript analysis.R
# RStudio: open this file and Source it.
# Quick data check: Rscript analysis.R --check-data
# Optional: --report exports the Random Forest/comparison/sensitivity LaTeX sections;
# --refresh checks existing results without retraining. Neither compiles a PDF.
# To replay raw-data cleaning, also provide ad.data and ad.names, then use --from-raw.
#
# Required packages: ggplot2, glmnet, randomForest, xgboost, PRROC, Matrix.
# Reference run: R 4.6.2, glmnet 5.2, randomForest 4.7-1.2,
# xgboost 3.2.1.1 and PRROC 1.4. Install packages before running this file.
# The script requires xgboost 3.2.1.1 for model consistency checks.
#
# Tables, figures and models are written to results/ beside this file.
# All analysis blocks are included below; no separate R/ folder is required.
# A full run includes cross-validation and 2000 bootstrap resamples and may take
# several minutes. Random Forest uses one worker for reproducibility.
#
# CSV columns: obs_id/y plus 1558 predictors and set/cv_fold/split_order.
# The last three columns preserve the shared split and are NOT predictors.
# NA remains NA; learned preprocessing is fitted inside training/CV only.
#
# Team: Quang (data), Trinh (statistics), Thang (Logistic),
# Quynh Mai (RF/comparison/sensitivity), Tai (XGBoost).
# R/... labels below identify embedded blocks, not files to download.
# Source markers support reproducibility checks and optional code excerpts.
# Native model metrics and common comparison metrics retain their stated definitions.

local({
  source_frames <- lapply(sys.frames(), function(x) x$ofile)
  source_frames <- Filter(function(x) is.character(x) && length(x) == 1L, source_frames)
  cli_file <- grep("^--file=", commandArgs(), value = TRUE)
  script <- if (length(source_frames)) tail(source_frames, 1L)[[1L]] else
    if (length(cli_file)) sub("^--file=", "", cli_file[1L]) else "analysis.R"
  .submission_file <- normalizePath(script, winslash = "/", mustWork = TRUE)
  .submission_root <- dirname(.submission_file)
  .submission_csv <- file.path(.submission_root, "cleaned_data.csv")
  .submission_modules <- new.env(parent = emptyenv())
  .submission_shared <- new.env(parent = environment())

  .submission_source <- function(file, local = FALSE, ...) {
    if (!is.character(file) || length(file) != 1L ||
        !exists(file, envir = .submission_modules, inherits = FALSE))
      stop("Unknown embedded analysis block: ", file, call. = FALSE)
    target <- if (is.environment(local)) local else
      if (isTRUE(local)) parent.frame() else .submission_shared
    eval(.submission_modules[[file]], envir = target)
    invisible(NULL)
  }
  .submission_input_paths <- function(paths) {
    mapped <- vapply(as.character(paths), function(p) {
      if (exists(p, envir = .submission_modules, inherits = FALSE)) .submission_file else p
    }, character(1), USE.NAMES = FALSE)
    unique(mapped)
  }
  .submission_source_lines <- function(path, ...) {
    if (!exists(path, envir = .submission_modules, inherits = FALSE))
      return(readLines(path, ...))
    lines <- readLines(.submission_file, warn = FALSE, encoding = "UTF-8")
    first <- which(lines == paste0("# BEGIN EMBEDDED SOURCE: ", path))
    last <- which(lines == paste0("# END EMBEDDED SOURCE: ", path))
    if (length(first) != 1L || length(last) != 1L || last <= first)
      stop("Embedded source markers are invalid: ", path)
    lines[seq.int(first + 2L, last - 2L)]
  }

  .submission_dictionary <- structure(list(column = c("height", "width", "aratio", "local",
"url_001", "url_002", "url_003", "url_004", "url_005", "url_006",
"url_007", "url_008", "url_009", "url_010", "url_011", "url_012",
"url_013", "url_014", "url_015", "url_016", "url_017", "url_018",
"url_019", "url_020", "url_021", "url_022", "url_023", "url_024",
"url_025", "url_026", "url_027", "url_028", "url_029", "url_030",
"url_031", "url_032", "url_033", "url_034", "url_035", "url_036",
"url_037", "url_038", "url_039", "url_040", "url_041", "url_042",
"url_043", "url_044", "url_045", "url_046", "url_047", "url_048",
"url_049", "url_050", "url_051", "url_052", "url_053", "url_054",
"url_055", "url_056", "url_057", "url_058", "url_059", "url_060",
"url_061", "url_062", "url_063", "url_064", "url_065", "url_066",
"url_067", "url_068", "url_069", "url_070", "url_071", "url_072",
"url_073", "url_074", "url_075", "url_076", "url_077", "url_078",
"url_079", "url_080", "url_081", "url_082", "url_083", "url_084",
"url_085", "url_086", "url_087", "url_088", "url_089", "url_090",
"url_091", "url_092", "url_093", "url_094", "url_095", "url_096",
"url_097", "url_098", "url_099", "url_100", "url_101", "url_102",
"url_103", "url_104", "url_105", "url_106", "url_107", "url_108",
"url_109", "url_110", "url_111", "url_112", "url_113", "url_114",
"url_115", "url_116", "url_117", "url_118", "url_119", "url_120",
"url_121", "url_122", "url_123", "url_124", "url_125", "url_126",
"url_127", "url_128", "url_129", "url_130", "url_131", "url_132",
"url_133", "url_134", "url_135", "url_136", "url_137", "url_138",
"url_139", "url_140", "url_141", "url_142", "url_143", "url_144",
"url_145", "url_146", "url_147", "url_148", "url_149", "url_150",
"url_151", "url_152", "url_153", "url_154", "url_155", "url_156",
"url_157", "url_158", "url_159", "url_160", "url_161", "url_162",
"url_163", "url_164", "url_165", "url_166", "url_167", "url_168",
"url_169", "url_170", "url_171", "url_172", "url_173", "url_174",
"url_175", "url_176", "url_177", "url_178", "url_179", "url_180",
"url_181", "url_182", "url_183", "url_184", "url_185", "url_186",
"url_187", "url_188", "url_189", "url_190", "url_191", "url_192",
"url_193", "url_194", "url_195", "url_196", "url_197", "url_198",
"url_199", "url_200", "url_201", "url_202", "url_203", "url_204",
"url_205", "url_206", "url_207", "url_208", "url_209", "url_210",
"url_211", "url_212", "url_213", "url_214", "url_215", "url_216",
"url_217", "url_218", "url_219", "url_220", "url_221", "url_222",
"url_223", "url_224", "url_225", "url_226", "url_227", "url_228",
"url_229", "url_230", "url_231", "url_232", "url_233", "url_234",
"url_235", "url_236", "url_237", "url_238", "url_239", "url_240",
"url_241", "url_242", "url_243", "url_244", "url_245", "url_246",
"url_247", "url_248", "url_249", "url_250", "url_251", "url_252",
"url_253", "url_254", "url_255", "url_256", "url_257", "url_258",
"url_259", "url_260", "url_261", "url_262", "url_263", "url_264",
"url_265", "url_266", "url_267", "url_268", "url_269", "url_270",
"url_271", "url_272", "url_273", "url_274", "url_275", "url_276",
"url_277", "url_278", "url_279", "url_280", "url_281", "url_282",
"url_283", "url_284", "url_285", "url_286", "url_287", "url_288",
"url_289", "url_290", "url_291", "url_292", "url_293", "url_294",
"url_295", "url_296", "url_297", "url_298", "url_299", "url_300",
"url_301", "url_302", "url_303", "url_304", "url_305", "url_306",
"url_307", "url_308", "url_309", "url_310", "url_311", "url_312",
"url_313", "url_314", "url_315", "url_316", "url_317", "url_318",
"url_319", "url_320", "url_321", "url_322", "url_323", "url_324",
"url_325", "url_326", "url_327", "url_328", "url_329", "url_330",
"url_331", "url_332", "url_333", "url_334", "url_335", "url_336",
"url_337", "url_338", "url_339", "url_340", "url_341", "url_342",
"url_343", "url_344", "url_345", "url_346", "url_347", "url_348",
"url_349", "url_350", "url_351", "url_352", "url_353", "url_354",
"url_355", "url_356", "url_357", "url_358", "url_359", "url_360",
"url_361", "url_362", "url_363", "url_364", "url_365", "url_366",
"url_367", "url_368", "url_369", "url_370", "url_371", "url_372",
"url_373", "url_374", "url_375", "url_376", "url_377", "url_378",
"url_379", "url_380", "url_381", "url_382", "url_383", "url_384",
"url_385", "url_386", "url_387", "url_388", "url_389", "url_390",
"url_391", "url_392", "url_393", "url_394", "url_395", "url_396",
"url_397", "url_398", "url_399", "url_400", "url_401", "url_402",
"url_403", "url_404", "url_405", "url_406", "url_407", "url_408",
"url_409", "url_410", "url_411", "url_412", "url_413", "url_414",
"url_415", "url_416", "url_417", "url_418", "url_419", "url_420",
"url_421", "url_422", "url_423", "url_424", "url_425", "url_426",
"url_427", "url_428", "url_429", "url_430", "url_431", "url_432",
"url_433", "url_434", "url_435", "url_436", "url_437", "url_438",
"url_439", "url_440", "url_441", "url_442", "url_443", "url_444",
"url_445", "url_446", "url_447", "url_448", "url_449", "url_450",
"url_451", "url_452", "url_453", "url_454", "url_455", "url_456",
"url_457", "origurl_001", "origurl_002", "origurl_003", "origurl_004",
"origurl_005", "origurl_006", "origurl_007", "origurl_008", "origurl_009",
"origurl_010", "origurl_011", "origurl_012", "origurl_013", "origurl_014",
"origurl_015", "origurl_016", "origurl_017", "origurl_018", "origurl_019",
"origurl_020", "origurl_021", "origurl_022", "origurl_023", "origurl_024",
"origurl_025", "origurl_026", "origurl_027", "origurl_028", "origurl_029",
"origurl_030", "origurl_031", "origurl_032", "origurl_033", "origurl_034",
"origurl_035", "origurl_036", "origurl_037", "origurl_038", "origurl_039",
"origurl_040", "origurl_041", "origurl_042", "origurl_043", "origurl_044",
"origurl_045", "origurl_046", "origurl_047", "origurl_048", "origurl_049",
"origurl_050", "origurl_051", "origurl_052", "origurl_053", "origurl_054",
"origurl_055", "origurl_056", "origurl_057", "origurl_058", "origurl_059",
"origurl_060", "origurl_061", "origurl_062", "origurl_063", "origurl_064",
"origurl_065", "origurl_066", "origurl_067", "origurl_068", "origurl_069",
"origurl_070", "origurl_071", "origurl_072", "origurl_073", "origurl_074",
"origurl_075", "origurl_076", "origurl_077", "origurl_078", "origurl_079",
"origurl_080", "origurl_081", "origurl_082", "origurl_083", "origurl_084",
"origurl_085", "origurl_086", "origurl_087", "origurl_088", "origurl_089",
"origurl_090", "origurl_091", "origurl_092", "origurl_093", "origurl_094",
"origurl_095", "origurl_096", "origurl_097", "origurl_098", "origurl_099",
"origurl_100", "origurl_101", "origurl_102", "origurl_103", "origurl_104",
"origurl_105", "origurl_106", "origurl_107", "origurl_108", "origurl_109",
"origurl_110", "origurl_111", "origurl_112", "origurl_113", "origurl_114",
"origurl_115", "origurl_116", "origurl_117", "origurl_118", "origurl_119",
"origurl_120", "origurl_121", "origurl_122", "origurl_123", "origurl_124",
"origurl_125", "origurl_126", "origurl_127", "origurl_128", "origurl_129",
"origurl_130", "origurl_131", "origurl_132", "origurl_133", "origurl_134",
"origurl_135", "origurl_136", "origurl_137", "origurl_138", "origurl_139",
"origurl_140", "origurl_141", "origurl_142", "origurl_143", "origurl_144",
"origurl_145", "origurl_146", "origurl_147", "origurl_148", "origurl_149",
"origurl_150", "origurl_151", "origurl_152", "origurl_153", "origurl_154",
"origurl_155", "origurl_156", "origurl_157", "origurl_158", "origurl_159",
"origurl_160", "origurl_161", "origurl_162", "origurl_163", "origurl_164",
"origurl_165", "origurl_166", "origurl_167", "origurl_168", "origurl_169",
"origurl_170", "origurl_171", "origurl_172", "origurl_173", "origurl_174",
"origurl_175", "origurl_176", "origurl_177", "origurl_178", "origurl_179",
"origurl_180", "origurl_181", "origurl_182", "origurl_183", "origurl_184",
"origurl_185", "origurl_186", "origurl_187", "origurl_188", "origurl_189",
"origurl_190", "origurl_191", "origurl_192", "origurl_193", "origurl_194",
"origurl_195", "origurl_196", "origurl_197", "origurl_198", "origurl_199",
"origurl_200", "origurl_201", "origurl_202", "origurl_203", "origurl_204",
"origurl_205", "origurl_206", "origurl_207", "origurl_208", "origurl_209",
"origurl_210", "origurl_211", "origurl_212", "origurl_213", "origurl_214",
"origurl_215", "origurl_216", "origurl_217", "origurl_218", "origurl_219",
"origurl_220", "origurl_221", "origurl_222", "origurl_223", "origurl_224",
"origurl_225", "origurl_226", "origurl_227", "origurl_228", "origurl_229",
"origurl_230", "origurl_231", "origurl_232", "origurl_233", "origurl_234",
"origurl_235", "origurl_236", "origurl_237", "origurl_238", "origurl_239",
"origurl_240", "origurl_241", "origurl_242", "origurl_243", "origurl_244",
"origurl_245", "origurl_246", "origurl_247", "origurl_248", "origurl_249",
"origurl_250", "origurl_251", "origurl_252", "origurl_253", "origurl_254",
"origurl_255", "origurl_256", "origurl_257", "origurl_258", "origurl_259",
"origurl_260", "origurl_261", "origurl_262", "origurl_263", "origurl_264",
"origurl_265", "origurl_266", "origurl_267", "origurl_268", "origurl_269",
"origurl_270", "origurl_271", "origurl_272", "origurl_273", "origurl_274",
"origurl_275", "origurl_276", "origurl_277", "origurl_278", "origurl_279",
"origurl_280", "origurl_281", "origurl_282", "origurl_283", "origurl_284",
"origurl_285", "origurl_286", "origurl_287", "origurl_288", "origurl_289",
"origurl_290", "origurl_291", "origurl_292", "origurl_293", "origurl_294",
"origurl_295", "origurl_296", "origurl_297", "origurl_298", "origurl_299",
"origurl_300", "origurl_301", "origurl_302", "origurl_303", "origurl_304",
"origurl_305", "origurl_306", "origurl_307", "origurl_308", "origurl_309",
"origurl_310", "origurl_311", "origurl_312", "origurl_313", "origurl_314",
"origurl_315", "origurl_316", "origurl_317", "origurl_318", "origurl_319",
"origurl_320", "origurl_321", "origurl_322", "origurl_323", "origurl_324",
"origurl_325", "origurl_326", "origurl_327", "origurl_328", "origurl_329",
"origurl_330", "origurl_331", "origurl_332", "origurl_333", "origurl_334",
"origurl_335", "origurl_336", "origurl_337", "origurl_338", "origurl_339",
"origurl_340", "origurl_341", "origurl_342", "origurl_343", "origurl_344",
"origurl_345", "origurl_346", "origurl_347", "origurl_348", "origurl_349",
"origurl_350", "origurl_351", "origurl_352", "origurl_353", "origurl_354",
"origurl_355", "origurl_356", "origurl_357", "origurl_358", "origurl_359",
"origurl_360", "origurl_361", "origurl_362", "origurl_363", "origurl_364",
"origurl_365", "origurl_366", "origurl_367", "origurl_368", "origurl_369",
"origurl_370", "origurl_371", "origurl_372", "origurl_373", "origurl_374",
"origurl_375", "origurl_376", "origurl_377", "origurl_378", "origurl_379",
"origurl_380", "origurl_381", "origurl_382", "origurl_383", "origurl_384",
"origurl_385", "origurl_386", "origurl_387", "origurl_388", "origurl_389",
"origurl_390", "origurl_391", "origurl_392", "origurl_393", "origurl_394",
"origurl_395", "origurl_396", "origurl_397", "origurl_398", "origurl_399",
"origurl_400", "origurl_401", "origurl_402", "origurl_403", "origurl_404",
"origurl_405", "origurl_406", "origurl_407", "origurl_408", "origurl_409",
"origurl_410", "origurl_411", "origurl_412", "origurl_413", "origurl_414",
"origurl_415", "origurl_416", "origurl_417", "origurl_418", "origurl_419",
"origurl_420", "origurl_421", "origurl_422", "origurl_423", "origurl_424",
"origurl_425", "origurl_426", "origurl_427", "origurl_428", "origurl_429",
"origurl_430", "origurl_431", "origurl_432", "origurl_433", "origurl_434",
"origurl_435", "origurl_436", "origurl_437", "origurl_438", "origurl_439",
"origurl_440", "origurl_441", "origurl_442", "origurl_443", "origurl_444",
"origurl_445", "origurl_446", "origurl_447", "origurl_448", "origurl_449",
"origurl_450", "origurl_451", "origurl_452", "origurl_453", "origurl_454",
"origurl_455", "origurl_456", "origurl_457", "origurl_458", "origurl_459",
"origurl_460", "origurl_461", "origurl_462", "origurl_463", "origurl_464",
"origurl_465", "origurl_466", "origurl_467", "origurl_468", "origurl_469",
"origurl_470", "origurl_471", "origurl_472", "origurl_473", "origurl_474",
"origurl_475", "origurl_476", "origurl_477", "origurl_478", "origurl_479",
"origurl_480", "origurl_481", "origurl_482", "origurl_483", "origurl_484",
"origurl_485", "origurl_486", "origurl_487", "origurl_488", "origurl_489",
"origurl_490", "origurl_491", "origurl_492", "origurl_493", "origurl_494",
"origurl_495", "ancurl_001", "ancurl_002", "ancurl_003", "ancurl_004",
"ancurl_005", "ancurl_006", "ancurl_007", "ancurl_008", "ancurl_009",
"ancurl_010", "ancurl_011", "ancurl_012", "ancurl_013", "ancurl_014",
"ancurl_015", "ancurl_016", "ancurl_017", "ancurl_018", "ancurl_019",
"ancurl_020", "ancurl_021", "ancurl_022", "ancurl_023", "ancurl_024",
"ancurl_025", "ancurl_026", "ancurl_027", "ancurl_028", "ancurl_029",
"ancurl_030", "ancurl_031", "ancurl_032", "ancurl_033", "ancurl_034",
"ancurl_035", "ancurl_036", "ancurl_037", "ancurl_038", "ancurl_039",
"ancurl_040", "ancurl_041", "ancurl_042", "ancurl_043", "ancurl_044",
"ancurl_045", "ancurl_046", "ancurl_047", "ancurl_048", "ancurl_049",
"ancurl_050", "ancurl_051", "ancurl_052", "ancurl_053", "ancurl_054",
"ancurl_055", "ancurl_056", "ancurl_057", "ancurl_058", "ancurl_059",
"ancurl_060", "ancurl_061", "ancurl_062", "ancurl_063", "ancurl_064",
"ancurl_065", "ancurl_066", "ancurl_067", "ancurl_068", "ancurl_069",
"ancurl_070", "ancurl_071", "ancurl_072", "ancurl_073", "ancurl_074",
"ancurl_075", "ancurl_076", "ancurl_077", "ancurl_078", "ancurl_079",
"ancurl_080", "ancurl_081", "ancurl_082", "ancurl_083", "ancurl_084",
"ancurl_085", "ancurl_086", "ancurl_087", "ancurl_088", "ancurl_089",
"ancurl_090", "ancurl_091", "ancurl_092", "ancurl_093", "ancurl_094",
"ancurl_095", "ancurl_096", "ancurl_097", "ancurl_098", "ancurl_099",
"ancurl_100", "ancurl_101", "ancurl_102", "ancurl_103", "ancurl_104",
"ancurl_105", "ancurl_106", "ancurl_107", "ancurl_108", "ancurl_109",
"ancurl_110", "ancurl_111", "ancurl_112", "ancurl_113", "ancurl_114",
"ancurl_115", "ancurl_116", "ancurl_117", "ancurl_118", "ancurl_119",
"ancurl_120", "ancurl_121", "ancurl_122", "ancurl_123", "ancurl_124",
"ancurl_125", "ancurl_126", "ancurl_127", "ancurl_128", "ancurl_129",
"ancurl_130", "ancurl_131", "ancurl_132", "ancurl_133", "ancurl_134",
"ancurl_135", "ancurl_136", "ancurl_137", "ancurl_138", "ancurl_139",
"ancurl_140", "ancurl_141", "ancurl_142", "ancurl_143", "ancurl_144",
"ancurl_145", "ancurl_146", "ancurl_147", "ancurl_148", "ancurl_149",
"ancurl_150", "ancurl_151", "ancurl_152", "ancurl_153", "ancurl_154",
"ancurl_155", "ancurl_156", "ancurl_157", "ancurl_158", "ancurl_159",
"ancurl_160", "ancurl_161", "ancurl_162", "ancurl_163", "ancurl_164",
"ancurl_165", "ancurl_166", "ancurl_167", "ancurl_168", "ancurl_169",
"ancurl_170", "ancurl_171", "ancurl_172", "ancurl_173", "ancurl_174",
"ancurl_175", "ancurl_176", "ancurl_177", "ancurl_178", "ancurl_179",
"ancurl_180", "ancurl_181", "ancurl_182", "ancurl_183", "ancurl_184",
"ancurl_185", "ancurl_186", "ancurl_187", "ancurl_188", "ancurl_189",
"ancurl_190", "ancurl_191", "ancurl_192", "ancurl_193", "ancurl_194",
"ancurl_195", "ancurl_196", "ancurl_197", "ancurl_198", "ancurl_199",
"ancurl_200", "ancurl_201", "ancurl_202", "ancurl_203", "ancurl_204",
"ancurl_205", "ancurl_206", "ancurl_207", "ancurl_208", "ancurl_209",
"ancurl_210", "ancurl_211", "ancurl_212", "ancurl_213", "ancurl_214",
"ancurl_215", "ancurl_216", "ancurl_217", "ancurl_218", "ancurl_219",
"ancurl_220", "ancurl_221", "ancurl_222", "ancurl_223", "ancurl_224",
"ancurl_225", "ancurl_226", "ancurl_227", "ancurl_228", "ancurl_229",
"ancurl_230", "ancurl_231", "ancurl_232", "ancurl_233", "ancurl_234",
"ancurl_235", "ancurl_236", "ancurl_237", "ancurl_238", "ancurl_239",
"ancurl_240", "ancurl_241", "ancurl_242", "ancurl_243", "ancurl_244",
"ancurl_245", "ancurl_246", "ancurl_247", "ancurl_248", "ancurl_249",
"ancurl_250", "ancurl_251", "ancurl_252", "ancurl_253", "ancurl_254",
"ancurl_255", "ancurl_256", "ancurl_257", "ancurl_258", "ancurl_259",
"ancurl_260", "ancurl_261", "ancurl_262", "ancurl_263", "ancurl_264",
"ancurl_265", "ancurl_266", "ancurl_267", "ancurl_268", "ancurl_269",
"ancurl_270", "ancurl_271", "ancurl_272", "ancurl_273", "ancurl_274",
"ancurl_275", "ancurl_276", "ancurl_277", "ancurl_278", "ancurl_279",
"ancurl_280", "ancurl_281", "ancurl_282", "ancurl_283", "ancurl_284",
"ancurl_285", "ancurl_286", "ancurl_287", "ancurl_288", "ancurl_289",
"ancurl_290", "ancurl_291", "ancurl_292", "ancurl_293", "ancurl_294",
"ancurl_295", "ancurl_296", "ancurl_297", "ancurl_298", "ancurl_299",
"ancurl_300", "ancurl_301", "ancurl_302", "ancurl_303", "ancurl_304",
"ancurl_305", "ancurl_306", "ancurl_307", "ancurl_308", "ancurl_309",
"ancurl_310", "ancurl_311", "ancurl_312", "ancurl_313", "ancurl_314",
"ancurl_315", "ancurl_316", "ancurl_317", "ancurl_318", "ancurl_319",
"ancurl_320", "ancurl_321", "ancurl_322", "ancurl_323", "ancurl_324",
"ancurl_325", "ancurl_326", "ancurl_327", "ancurl_328", "ancurl_329",
"ancurl_330", "ancurl_331", "ancurl_332", "ancurl_333", "ancurl_334",
"ancurl_335", "ancurl_336", "ancurl_337", "ancurl_338", "ancurl_339",
"ancurl_340", "ancurl_341", "ancurl_342", "ancurl_343", "ancurl_344",
"ancurl_345", "ancurl_346", "ancurl_347", "ancurl_348", "ancurl_349",
"ancurl_350", "ancurl_351", "ancurl_352", "ancurl_353", "ancurl_354",
"ancurl_355", "ancurl_356", "ancurl_357", "ancurl_358", "ancurl_359",
"ancurl_360", "ancurl_361", "ancurl_362", "ancurl_363", "ancurl_364",
"ancurl_365", "ancurl_366", "ancurl_367", "ancurl_368", "ancurl_369",
"ancurl_370", "ancurl_371", "ancurl_372", "ancurl_373", "ancurl_374",
"ancurl_375", "ancurl_376", "ancurl_377", "ancurl_378", "ancurl_379",
"ancurl_380", "ancurl_381", "ancurl_382", "ancurl_383", "ancurl_384",
"ancurl_385", "ancurl_386", "ancurl_387", "ancurl_388", "ancurl_389",
"ancurl_390", "ancurl_391", "ancurl_392", "ancurl_393", "ancurl_394",
"ancurl_395", "ancurl_396", "ancurl_397", "ancurl_398", "ancurl_399",
"ancurl_400", "ancurl_401", "ancurl_402", "ancurl_403", "ancurl_404",
"ancurl_405", "ancurl_406", "ancurl_407", "ancurl_408", "ancurl_409",
"ancurl_410", "ancurl_411", "ancurl_412", "ancurl_413", "ancurl_414",
"ancurl_415", "ancurl_416", "ancurl_417", "ancurl_418", "ancurl_419",
"ancurl_420", "ancurl_421", "ancurl_422", "ancurl_423", "ancurl_424",
"ancurl_425", "ancurl_426", "ancurl_427", "ancurl_428", "ancurl_429",
"ancurl_430", "ancurl_431", "ancurl_432", "ancurl_433", "ancurl_434",
"ancurl_435", "ancurl_436", "ancurl_437", "ancurl_438", "ancurl_439",
"ancurl_440", "ancurl_441", "ancurl_442", "ancurl_443", "ancurl_444",
"ancurl_445", "ancurl_446", "ancurl_447", "ancurl_448", "ancurl_449",
"ancurl_450", "ancurl_451", "ancurl_452", "ancurl_453", "ancurl_454",
"ancurl_455", "ancurl_456", "ancurl_457", "ancurl_458", "ancurl_459",
"ancurl_460", "ancurl_461", "ancurl_462", "ancurl_463", "ancurl_464",
"ancurl_465", "ancurl_466", "ancurl_467", "ancurl_468", "ancurl_469",
"ancurl_470", "ancurl_471", "ancurl_472", "alt_001", "alt_002",
"alt_003", "alt_004", "alt_005", "alt_006", "alt_007", "alt_008",
"alt_009", "alt_010", "alt_011", "alt_012", "alt_013", "alt_014",
"alt_015", "alt_016", "alt_017", "alt_018", "alt_019", "alt_020",
"alt_021", "alt_022", "alt_023", "alt_024", "alt_025", "alt_026",
"alt_027", "alt_028", "alt_029", "alt_030", "alt_031", "alt_032",
"alt_033", "alt_034", "alt_035", "alt_036", "alt_037", "alt_038",
"alt_039", "alt_040", "alt_041", "alt_042", "alt_043", "alt_044",
"alt_045", "alt_046", "alt_047", "alt_048", "alt_049", "alt_050",
"alt_051", "alt_052", "alt_053", "alt_054", "alt_055", "alt_056",
"alt_057", "alt_058", "alt_059", "alt_060", "alt_061", "alt_062",
"alt_063", "alt_064", "alt_065", "alt_066", "alt_067", "alt_068",
"alt_069", "alt_070", "alt_071", "alt_072", "alt_073", "alt_074",
"alt_075", "alt_076", "alt_077", "alt_078", "alt_079", "alt_080",
"alt_081", "alt_082", "alt_083", "alt_084", "alt_085", "alt_086",
"alt_087", "alt_088", "alt_089", "alt_090", "alt_091", "alt_092",
"alt_093", "alt_094", "alt_095", "alt_096", "alt_097", "alt_098",
"alt_099", "alt_100", "alt_101", "alt_102", "alt_103", "alt_104",
"alt_105", "alt_106", "alt_107", "alt_108", "alt_109", "alt_110",
"alt_111", "caption_001", "caption_002", "caption_003", "caption_004",
"caption_005", "caption_006", "caption_007", "caption_008", "caption_009",
"caption_010", "caption_011", "caption_012", "caption_013", "caption_014",
"caption_015", "caption_016", "caption_017", "caption_018", "caption_019"
), group = c("geometry", "geometry", "geometry", "local", "url",
"url", "url", "url", "url", "url", "url", "url", "url", "url",
"url", "url", "url", "url", "url", "url", "url", "url", "url",
"url", "url", "url", "url", "url", "url", "url", "url", "url",
"url", "url", "url", "url", "url", "url", "url", "url", "url",
"url", "url", "url", "url", "url", "url", "url", "url", "url",
"url", "url", "url", "url", "url", "url", "url", "url", "url",
"url", "url", "url", "url", "url", "url", "url", "url", "url",
"url", "url", "url", "url", "url", "url", "url", "url", "url",
"url", "url", "url", "url", "url", "url", "url", "url", "url",
"url", "url", "url", "url", "url", "url", "url", "url", "url",
"url", "url", "url", "url", "url", "url", "url", "url", "url",
"url", "url", "url", "url", "url", "url", "url", "url", "url",
"url", "url", "url", "url", "url", "url", "url", "url", "url",
"url", "url", "url", "url", "url", "url", "url", "url", "url",
"url", "url", "url", "url", "url", "url", "url", "url", "url",
"url", "url", "url", "url", "url", "url", "url", "url", "url",
"url", "url", "url", "url", "url", "url", "url", "url", "url",
"url", "url", "url", "url", "url", "url", "url", "url", "url",
"url", "url", "url", "url", "url", "url", "url", "url", "url",
"url", "url", "url", "url", "url", "url", "url", "url", "url",
"url", "url", "url", "url", "url", "url", "url", "url", "url",
"url", "url", "url", "url", "url", "url", "url", "url", "url",
"url", "url", "url", "url", "url", "url", "url", "url", "url",
"url", "url", "url", "url", "url", "url", "url", "url", "url",
"url", "url", "url", "url", "url", "url", "url", "url", "url",
"url", "url", "url", "url", "url", "url", "url", "url", "url",
"url", "url", "url", "url", "url", "url", "url", "url", "url",
"url", "url", "url", "url", "url", "url", "url", "url", "url",
"url", "url", "url", "url", "url", "url", "url", "url", "url",
"url", "url", "url", "url", "url", "url", "url", "url", "url",
"url", "url", "url", "url", "url", "url", "url", "url", "url",
"url", "url", "url", "url", "url", "url", "url", "url", "url",
"url", "url", "url", "url", "url", "url", "url", "url", "url",
"url", "url", "url", "url", "url", "url", "url", "url", "url",
"url", "url", "url", "url", "url", "url", "url", "url", "url",
"url", "url", "url", "url", "url", "url", "url", "url", "url",
"url", "url", "url", "url", "url", "url", "url", "url", "url",
"url", "url", "url", "url", "url", "url", "url", "url", "url",
"url", "url", "url", "url", "url", "url", "url", "url", "url",
"url", "url", "url", "url", "url", "url", "url", "url", "url",
"url", "url", "url", "url", "url", "url", "url", "url", "url",
"url", "url", "url", "url", "url", "url", "url", "url", "url",
"url", "url", "url", "url", "url", "url", "url", "url", "url",
"url", "url", "url", "url", "url", "url", "url", "url", "url",
"url", "url", "url", "url", "url", "url", "url", "url", "url",
"url", "url", "url", "url", "url", "url", "url", "url", "url",
"url", "url", "url", "url", "url", "url", "url", "url", "url",
"url", "url", "url", "url", "url", "url", "url", "url", "url",
"url", "url", "url", "url", "url", "url", "url", "url", "url",
"url", "url", "url", "url", "url", "url", "url", "url", "url",
"url", "url", "url", "url", "url", "url", "origurl", "origurl",
"origurl", "origurl", "origurl", "origurl", "origurl", "origurl",
"origurl", "origurl", "origurl", "origurl", "origurl", "origurl",
"origurl", "origurl", "origurl", "origurl", "origurl", "origurl",
"origurl", "origurl", "origurl", "origurl", "origurl", "origurl",
"origurl", "origurl", "origurl", "origurl", "origurl", "origurl",
"origurl", "origurl", "origurl", "origurl", "origurl", "origurl",
"origurl", "origurl", "origurl", "origurl", "origurl", "origurl",
"origurl", "origurl", "origurl", "origurl", "origurl", "origurl",
"origurl", "origurl", "origurl", "origurl", "origurl", "origurl",
"origurl", "origurl", "origurl", "origurl", "origurl", "origurl",
"origurl", "origurl", "origurl", "origurl", "origurl", "origurl",
"origurl", "origurl", "origurl", "origurl", "origurl", "origurl",
"origurl", "origurl", "origurl", "origurl", "origurl", "origurl",
"origurl", "origurl", "origurl", "origurl", "origurl", "origurl",
"origurl", "origurl", "origurl", "origurl", "origurl", "origurl",
"origurl", "origurl", "origurl", "origurl", "origurl", "origurl",
"origurl", "origurl", "origurl", "origurl", "origurl", "origurl",
"origurl", "origurl", "origurl", "origurl", "origurl", "origurl",
"origurl", "origurl", "origurl", "origurl", "origurl", "origurl",
"origurl", "origurl", "origurl", "origurl", "origurl", "origurl",
"origurl", "origurl", "origurl", "origurl", "origurl", "origurl",
"origurl", "origurl", "origurl", "origurl", "origurl", "origurl",
"origurl", "origurl", "origurl", "origurl", "origurl", "origurl",
"origurl", "origurl", "origurl", "origurl", "origurl", "origurl",
"origurl", "origurl", "origurl", "origurl", "origurl", "origurl",
"origurl", "origurl", "origurl", "origurl", "origurl", "origurl",
"origurl", "origurl", "origurl", "origurl", "origurl", "origurl",
"origurl", "origurl", "origurl", "origurl", "origurl", "origurl",
"origurl", "origurl", "origurl", "origurl", "origurl", "origurl",
"origurl", "origurl", "origurl", "origurl", "origurl", "origurl",
"origurl", "origurl", "origurl", "origurl", "origurl", "origurl",
"origurl", "origurl", "origurl", "origurl", "origurl", "origurl",
"origurl", "origurl", "origurl", "origurl", "origurl", "origurl",
"origurl", "origurl", "origurl", "origurl", "origurl", "origurl",
"origurl", "origurl", "origurl", "origurl", "origurl", "origurl",
"origurl", "origurl", "origurl", "origurl", "origurl", "origurl",
"origurl", "origurl", "origurl", "origurl", "origurl", "origurl",
"origurl", "origurl", "origurl", "origurl", "origurl", "origurl",
"origurl", "origurl", "origurl", "origurl", "origurl", "origurl",
"origurl", "origurl", "origurl", "origurl", "origurl", "origurl",
"origurl", "origurl", "origurl", "origurl", "origurl", "origurl",
"origurl", "origurl", "origurl", "origurl", "origurl", "origurl",
"origurl", "origurl", "origurl", "origurl", "origurl", "origurl",
"origurl", "origurl", "origurl", "origurl", "origurl", "origurl",
"origurl", "origurl", "origurl", "origurl", "origurl", "origurl",
"origurl", "origurl", "origurl", "origurl", "origurl", "origurl",
"origurl", "origurl", "origurl", "origurl", "origurl", "origurl",
"origurl", "origurl", "origurl", "origurl", "origurl", "origurl",
"origurl", "origurl", "origurl", "origurl", "origurl", "origurl",
"origurl", "origurl", "origurl", "origurl", "origurl", "origurl",
"origurl", "origurl", "origurl", "origurl", "origurl", "origurl",
"origurl", "origurl", "origurl", "origurl", "origurl", "origurl",
"origurl", "origurl", "origurl", "origurl", "origurl", "origurl",
"origurl", "origurl", "origurl", "origurl", "origurl", "origurl",
"origurl", "origurl", "origurl", "origurl", "origurl", "origurl",
"origurl", "origurl", "origurl", "origurl", "origurl", "origurl",
"origurl", "origurl", "origurl", "origurl", "origurl", "origurl",
"origurl", "origurl", "origurl", "origurl", "origurl", "origurl",
"origurl", "origurl", "origurl", "origurl", "origurl", "origurl",
"origurl", "origurl", "origurl", "origurl", "origurl", "origurl",
"origurl", "origurl", "origurl", "origurl", "origurl", "origurl",
"origurl", "origurl", "origurl", "origurl", "origurl", "origurl",
"origurl", "origurl", "origurl", "origurl", "origurl", "origurl",
"origurl", "origurl", "origurl", "origurl", "origurl", "origurl",
"origurl", "origurl", "origurl", "origurl", "origurl", "origurl",
"origurl", "origurl", "origurl", "origurl", "origurl", "origurl",
"origurl", "origurl", "origurl", "origurl", "origurl", "origurl",
"origurl", "origurl", "origurl", "origurl", "origurl", "origurl",
"origurl", "origurl", "origurl", "origurl", "origurl", "origurl",
"origurl", "origurl", "origurl", "origurl", "origurl", "origurl",
"origurl", "origurl", "origurl", "origurl", "origurl", "origurl",
"origurl", "origurl", "origurl", "origurl", "origurl", "origurl",
"origurl", "origurl", "origurl", "origurl", "origurl", "origurl",
"origurl", "origurl", "origurl", "origurl", "origurl", "origurl",
"origurl", "origurl", "origurl", "origurl", "origurl", "origurl",
"origurl", "origurl", "origurl", "origurl", "origurl", "origurl",
"origurl", "origurl", "origurl", "origurl", "origurl", "origurl",
"origurl", "origurl", "origurl", "origurl", "origurl", "origurl",
"origurl", "origurl", "origurl", "origurl", "origurl", "origurl",
"origurl", "origurl", "origurl", "origurl", "origurl", "origurl",
"origurl", "origurl", "origurl", "origurl", "origurl", "origurl",
"origurl", "origurl", "origurl", "origurl", "origurl", "origurl",
"origurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl",
"ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl",
"ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl",
"ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl",
"ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl",
"ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl",
"ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl",
"ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl",
"ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl",
"ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl",
"ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl",
"ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl",
"ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl",
"ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl",
"ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl",
"ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl",
"ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl",
"ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl",
"ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl",
"ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl",
"ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl",
"ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl",
"ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl",
"ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl",
"ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl",
"ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl",
"ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl",
"ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl",
"ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl",
"ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl",
"ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl",
"ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl",
"ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl",
"ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl",
"ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl",
"ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl",
"ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl",
"ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl",
"ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl",
"ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl",
"ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl",
"ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl",
"ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl",
"ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl",
"ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl",
"ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl",
"ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl",
"ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl",
"ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl",
"ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl",
"ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl",
"ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl",
"ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl",
"ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl",
"ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl",
"ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl",
"ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl",
"ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl",
"ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl",
"ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl",
"ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl",
"ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl",
"ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl",
"ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl",
"ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl",
"ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl",
"ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "ancurl",
"ancurl", "ancurl", "ancurl", "ancurl", "ancurl", "alt", "alt",
"alt", "alt", "alt", "alt", "alt", "alt", "alt", "alt", "alt",
"alt", "alt", "alt", "alt", "alt", "alt", "alt", "alt", "alt",
"alt", "alt", "alt", "alt", "alt", "alt", "alt", "alt", "alt",
"alt", "alt", "alt", "alt", "alt", "alt", "alt", "alt", "alt",
"alt", "alt", "alt", "alt", "alt", "alt", "alt", "alt", "alt",
"alt", "alt", "alt", "alt", "alt", "alt", "alt", "alt", "alt",
"alt", "alt", "alt", "alt", "alt", "alt", "alt", "alt", "alt",
"alt", "alt", "alt", "alt", "alt", "alt", "alt", "alt", "alt",
"alt", "alt", "alt", "alt", "alt", "alt", "alt", "alt", "alt",
"alt", "alt", "alt", "alt", "alt", "alt", "alt", "alt", "alt",
"alt", "alt", "alt", "alt", "alt", "alt", "alt", "alt", "alt",
"alt", "alt", "alt", "alt", "alt", "alt", "alt", "alt", "alt",
"alt", "caption", "caption", "caption", "caption", "caption",
"caption", "caption", "caption", "caption", "caption", "caption",
"caption", "caption", "caption", "caption", "caption", "caption",
"caption", "caption"), original_name = c("height", "width", "aratio",
"local", "url*images+buttons", "url*likesbooks.com", "url*www.slake.com",
"url*hydrogeologist", "url*oso", "url*media", "url*peace+images",
"url*blipverts", "url*tkaine+kats", "url*labyrinth", "url*advertising+blipverts",
"url*images+oso", "url*area51+corridor", "url*ran+gifs", "url*express-scripts.com",
"url*off", "url*cnet", "url*time+1998", "url*josefina3", "url*truluck.com",
"url*clawnext+gif", "url*autopen.com", "url*tvgen.com", "url*pixs",
"url*heartland+5309", "url*meadows+9196", "url*blue", "url*ad+gif",
"url*area51", "url*www.internauts.ca", "url*afn.org", "url*ran.org",
"url*shareware.com", "url*baons+images", "url*area51+labyrinth",
"url*pics", "url*image+navigate", "url*pool+images", "url*carousel.org",
"url*members.accessus.net", "url*cmp+flowsoft010", "url*smallb+gif",
"url*www.FlowSoft.com", "url*kandinsky", "url*csuhayward.edu",
"url*www.yahoo.co.uk", "url*ibitexas.com", "url*www.haestad.com",
"url*antiwar.com", "url*www.monmouth.com", "url*infoserver.etl.vt.edu",
"url*geoguideii+tour", "url*heartland+pointe", "url*romancebooks+pix",
"url*468x60", "url*ads.switchboard.com", "url*keith+dumble",
"url*cqi.com", "url*images+ixfolder", "url*chat+b", "url*images+geoguideii",
"url*preferences.com", "url*icsi.com", "url*pacific.net.sg",
"url*monmouth.com", "url*wired.com", "url*go2net", "url*smiley+gif",
"url*navigate+b", "url*sunsite.unc.edu", "url*www.pacific.net.sg",
"url*home+gif", "url*library", "url*puc.edu", "url*internauts.ca",
"url*www.worlddrugstore.com", "url*www.ibitexas.com", "url*ucsd.edu",
"url*pitcairn+smallb", "url*kats", "url*cjackson+kandinsky",
"url*tkaine+rings", "url*any+time", "url*FlowSoft.com", "url*geoguideii+nh",
"url*charlie", "url*heartland+meadows", "url*home.netscape.com",
"url*9439+butt2", "url*catringn+gif", "url*martnet", "url*doubleclick.net",
"url*lycos.de", "url*catringn", "url*catring1", "url*fools+pixs",
"url*logos", "url*private", "url*ng+spacedesc", "url*larx.com",
"url*site", "url*pointe+1154", "url*banners", "url*pixs+smiley",
"url*www.123greetings.com", "url*www.inwap.com", "url*cjackson",
"url*labyrinth+9439", "url*generated+images", "url*image+ng",
"url*lolnad+private", "url*library+pitcairn", "url*www.mei.co.jp",
"url*april", "url*derived", "url*smiley", "url*home", "url*jsd",
"url*sonic.net", "url*send+b", "url*website.ora.com", "url*pawbutton+gif",
"url*victoria+pharmacy", "url*keith", "url*pages", "url*worlddrugstore.com",
"url*geoguideii+email", "url*www.icgnet.com", "url*forum", "url*www.afn.org",
"url*time", "url*www.ran.org", "url*main", "url*members+keith",
"url*geoguideii+pages", "url*banner", "url*stinky+gra", "url*tkaine+buttons",
"url*victoria", "url*kbell", "url*www.truluck.com", "url*img",
"url*pics+cats", "url*alley", "url*claw1", "url*pharmacy+gif",
"url*www.autopen.com", "url*images+img", "url*fbox.vt.edu", "url*unc.edu",
"url*ics", "url*times", "url*forums", "url*chat", "url*images.go2net.com",
"url*users.aol.com", "url*malek", "url*jsd+how", "url*www.cqi.com",
"url*advertising+graphics", "url*pawbutton", "url*pictures",
"url*grouper.com", "url*ads+media", "url*www.martnet.com", "url*set+butt",
"url*how", "url*www.wednet.com", "url*www.globec.com.au", "url*athens",
"url*info", "url*www.ireland-today.ie", "url*grn+bull", "url*sjsu.edu",
"url*www.antiwar.com", "url*www.puc.edu", "url*sponsor", "url*www.news.com",
"url*flowsoft010", "url*memberbanners", "url*red+ball", "url*ads.msn.com",
"url*icons", "url*images", "url*access.ch", "url*irish+times",
"url*media+images", "url*www.adamspharmacy.com", "url*geocities.com",
"url*www.uk.lycos.de", "url*copymat.com", "url*hem1.passagen.se",
"url*meadows", "url*grn", "url*gra", "url*vt.edu", "url*www.express-scripts.com",
"url*imgs", "url*personal", "url*mindlink.net", "url*send", "url*pages+b",
"url*accessus.net", "url*www.finest.tm.fr", "url*geoguideii+search",
"url*polypkem", "url*fools", "url*www.larx.com", "url*marketplace",
"url*wco.com", "url*off+gif", "url*tkaine+bars", "url*icgnet.com",
"url*www.autotown.com", "url*generated", "url*9196+pawbutton",
"url*geoguideii+chat", "url*www.vrn.de", "url*claw1+gif", "url*theinternetadvantage.com",
"url*runofsite", "url*romancebooks", "url*foxy.net", "url*francois",
"url*links", "url*athens+forum", "url*autotown.com", "url*logo",
"url*members", "url*auto", "url*afn47757", "url*gifs", "url*esi",
"url*hollywood", "url*forum+8078", "url*dept", "url*adamspharmacy.com",
"url*valley", "url*www.pacificrim.net", "url*about", "url*www.likesbooks.com",
"url*bossintl.com", "url*images+home", "url*wednet.com", "url*news.com",
"url*arvann+images", "url*users", "url*netscape.com", "url*athens+8774",
"url*pictures+gc", "url*butt", "url*e+gif", "url*sunsetstrip+alley",
"url*geoguideii+send", "url*catring1+gif", "url*ball+gif", "url*bull",
"url*paul.spu.edu", "url*rank", "url*2meta.com", "url*w+gif",
"url*aol.com", "url*lg+gif", "url*irish-times.com", "url*irish",
"url*ad.doubleclick.net", "url*ad.preferences.com", "url*bars",
"url*forums+b", "url*gc+icon", "url*www.geocities.com", "url*www.carousel.org",
"url*ball", "url*spu.edu", "url*com", "url*cnm", "url*htm+images",
"url*htm+cmp", "url*cmp", "url*link+pics", "url*mindspring.com",
"url*geoguideii+logo", "url*psu.edu", "url*go2net+ads", "url*www.grouper.com",
"url*butt2", "url*cat", "url*www.dreamn.com", "url*vbtn+gif",
"url*switchboard.com", "url*logo+b", "url*assets+images", "url*martnet.com",
"url*buttons", "url*thejeep.com", "url*auto+generated", "url*search+b",
"url*search", "url*pharmacy", "url*assets+auto", "url*static.wired.com",
"url*cats", "url*bull+gif", "url*memberbanners+live", "url*pitcairn",
"url*www.access.ch", "url*adv+images", "url*navigate", "url*runofsite+any",
"url*dejay", "url*bishop", "url*email+b", "url*www.mcs.csuhayward.edu",
"url*bin", "url*paris", "url*1154+graypaw", "url*stinky", "url*img+ads",
"url*bds", "url*www.wco.com", "url*esi+image", "url*vrn.de",
"url*www.toyotaofroswell.com", "url*www.copymat.com", "url*www.psnw.com",
"url*go2net.com", "url*advertising", "url*slake.com", "url*www.theinternetadvantage.com",
"url*baons", "url*geoguideii+rank", "url*butt2+gif", "url*tkaine",
"url*art", "url*lovisa1", "url*pool", "url*valley+2647", "url*any",
"url*corridor", "url*top", "url*rank+b", "url*pacificrim.net",
"url*image", "url*heartland+valley", "url*tii", "url*123greetings.com",
"url*adv", "url*ads", "url*hydrogeologist+imgs", "url*smallb",
"url*members.tripod.com", "url*www.shareware.com", "url*images+ads",
"url*banner+gif", "url*vbtn", "url*globec.com.au", "url*www.news-observer.com",
"url*pointe", "url*mandypaul", "url*bds+s", "url*www.ems.psu.edu",
"url*valley+2539", "url*members.aol.com", "url*yahoo.co.uk",
"url*malek+small", "url*button", "url*msn.com", "url*graypaw+gif",
"url*live", "url*haestad.com", "url*sanjosesabercats.com", "url*icon+gif",
"url*april+fools", "url*assets", "url*set", "url*link", "url*wc",
"url*uk", "url*sj", "url*images+set", "url*www.bossintl.com",
"url*nh", "url*ng", "url*counter", "url*lg", "url*spacedesc",
"url*bishop+red", "url*it", "url*ie", "url*rings", "url*gc",
"url*peace", "url*dreamn.com", "url*ar", "url*ad", "url*thumb",
"url*clawnext", "url*dumble", "url*kandinsky+t", "url*email",
"url*martnet+images", "url*uk+gif", "url*red", "url*geoguideii+forums",
"url*www.2meta.com", "url*ran", "url*ukonline.co.uk", "url*passagen.se",
"url*psnw.com", "url*how+htm", "url*tm.fr", "url*sunsetstrip",
"url*ora.com", "url*logo+gif", "url*marketing", "url*toyotaofroswell.com",
"url*heartland", "url*ixfolder+gif", "url*mcet.edu", "url*enchantedforest",
"url*alley+6750", "url*news-observer.com", "url*ireland-today.ie",
"url*times+graphics", "url*madamecleo", "url*b+gif", "url*www.irish-times.com",
"url*tripod.com", "url*ad+icons", "url*button+gif", "url*graypaw",
"url*excite.com", "url*web.ukonline.co.uk", "url*polypkem+grn",
"url*gort.ucsd.edu", "url*lolnad", "url*graphics+ad", "url*icon",
"url*falcon.sonic.net", "url*picts", "url*phoenix.mcet.edu",
"url*pix+b", "url*www.icsi.com", "url*ixfolder", "url*inwap.com",
"url*www.cob.sjsu.edu", "url*pix", "url*www.thejeep.com", "url*tour",
"url*www.sanjosesabercats.com", "url*mei.co.jp", "url*geoguideii",
"url*small", "url*graphics", "url*kbell+charlie", "url*zueri.ch",
"url*arvann", "origurl*labyrinth", "origurl*puc.edu", "origurl*charlie+charlie",
"origurl*hevern+psychref", "origurl*and", "origurl*pterry+htm",
"origurl*bishop", "origurl*ora.com", "origurl*www.nyx.net", "origurl*www.yahoo.co.uk",
"origurl*www.truluck.com", "origurl*target+\302\256ion", "origurl*tkaine+sydkat",
"origurl*meadows+9196", "origurl*top", "origurl*members.xoom.com",
"origurl*0+rpp", "origurl*www.thriveonline.com", "origurl*adamspharmacy.com",
"origurl*icsi.com", "origurl*tkaine+stvkat", "origurl*shareware.com",
"origurl*e+html", "origurl*www.123greetings.com", "origurl*lofts+7119",
"origurl*likesbooks.com", "origurl*gort.ucsd.edu", "origurl*users.aol.com",
"origurl*www.innotts.co.uk", "origurl*roll.com", "origurl*web.ukonline.co.uk",
"origurl*events", "origurl*www.2meta.com", "origurl*chuck+html",
"origurl*paris", "origurl*chapel", "origurl*tii", "origurl*how+title",
"origurl*www.news-observer.com", "origurl*alley", "origurl*april+fools",
"origurl*carousel", "origurl*www.haestad.com", "origurl*title+html",
"origurl*lemoyne.edu", "origurl*plains+5257", "origurl*psychref",
"origurl*jun", "origurl*www.interchg.ubc.ca", "origurl*ukonline.co.uk",
"origurl*www.lycos.co.uk", "origurl*lolnad+private", "origurl*ibitexas.com",
"origurl*wco.com", "origurl*jsd", "origurl*www.rock.n.roll.com",
"origurl*antiwar.com", "origurl*target", "origurl*phoenix.mcet.edu",
"origurl*index", "origurl*1154+index", "origurl*pharmacy", "origurl*coltrane",
"origurl*website.ora.com", "origurl*index+htm", "origurl*dept",
"origurl*area51+labyrinth", "origurl*irish-times.com", "origurl*mandypaul",
"origurl*pointe", "origurl*plato+htm", "origurl*arvann", "origurl*\302\256ion+0",
"origurl*chapel+safehaven", "origurl*worldvillage.com", "origurl*nf+htm",
"origurl*www.martnet.com", "origurl*hem1.passagen.se", "origurl*worlddrugstore.com",
"origurl*www.globec.com.au", "origurl*athens+8774", "origurl*times+paper",
"origurl*actup", "origurl*inwap", "origurl*main", "origurl*www.larx.com",
"origurl*dumble+contents", "origurl*geocities.com", "origurl*4301+misfits2",
"origurl*kattsida", "origurl*members.aol.com", "origurl*spu.edu",
"origurl*sunsite.unc.edu", "origurl*valley+2647", "origurl*josefina3",
"origurl*tkaine", "origurl*bonchin+cat", "origurl*chopin+htm",
"origurl*punkie", "origurl*ubc.ca", "origurl*links+nf", "origurl*misfits2",
"origurl*area51+vault", "origurl*kitty", "origurl*plato", "origurl*hollywood+7735",
"origurl*link", "origurl*www.monmouth.com", "origurl*hist", "origurl*members.tripod.com",
"origurl*www.ibitexas.com", "origurl*cgidir+dll", "origurl*heartland",
"origurl*pages+catscats", "origurl*tkaine+linda", "origurl*crawler",
"origurl*paul.spu.edu", "origurl*bishop+pterry", "origurl*carousel.org",
"origurl*romance", "origurl*nyx.net", "origurl*hollywood", "origurl*simonc",
"origurl*sox", "origurl*www.cob.sjsu.edu", "origurl*timeout+5",
"origurl*leonora", "origurl*fh-rpl.de", "origurl*vt.edu", "origurl*ucsd.edu",
"origurl*www.carousel.org", "origurl*www.antiwar.com", "origurl*FlowSoft.com",
"origurl*kbell+charlie", "origurl*www.likesbooks.com", "origurl*labyrinth+9439",
"origurl*www.icsi.com", "origurl*events+aprilfoolsday", "origurl*tii+tii",
"origurl*athens+forum", "origurl*library", "origurl*valley",
"origurl*ics+ikenobo", "origurl*unc.edu", "origurl*kandin", "origurl*bordeaux",
"origurl*charlie", "origurl*6750+paws", "origurl*cccats", "origurl*linda+htm",
"origurl*2647+coltrane", "origurl*vault+3440", "origurl*victoria+pharmacy",
"origurl*wv", "origurl*kandin+html", "origurl*soho+lofts", "origurl*sunsetstrip+alley",
"origurl*slake.com", "origurl*autotown.com", "origurl*www.sanjosesabercats.com",
"origurl*infoserver.etl.vt.edu", "origurl*kushmerick", "origurl*www.toyotaofroswell.com",
"origurl*turbo", "origurl*sw", "origurl*hills", "origurl*st",
"origurl*mogwhi", "origurl*sj", "origurl*pointe+1154", "origurl*monmouth.com",
"origurl*mei.co.jp", "origurl*sonic.net", "origurl*safehaven+index",
"origurl*simonc+cybermog", "origurl*messier", "origurl*cybercats",
"origurl*search", "origurl*heartland+5309", "origurl*psnw.com",
"origurl*keith+dumble", "origurl*links", "origurl*nav", "origurl*nf",
"origurl*sunsetstrip", "origurl*kandinsky", "origurl*meadows+3727",
"origurl*www.afn.org", "origurl*southbeach", "origurl*pacific.net.sg",
"origurl*heartland+pointe", "origurl*valley+2539", "origurl*catscats+html",
"origurl*horn+html", "origurl*area51", "origurl*pages", "origurl*chopin",
"origurl*it", "origurl*bonchin", "origurl*www.worldvillage.com",
"origurl*00+html", "origurl*vrn.de", "origurl*home.netscape.com",
"origurl*simstory+html", "origurl*biopic+htm", "origurl*dll",
"origurl*cmeese+malibu", "origurl*ics", "origurl*linda", "origurl*fr",
"origurl*private+biopic", "origurl*corridor+4590", "origurl*leonora+html",
"origurl*dejay", "origurl*aprilfoolsday", "origurl*fools", "origurl*biopic",
"origurl*lofts", "origurl*heartland+plains", "origurl*passagen.se",
"origurl*djang", "origurl*5257+timstory", "origurl*5+hpe", "origurl*cybermog+htm",
"origurl*area51+corridor", "origurl*yosemite+4301", "origurl*wosc+html",
"origurl*rpp", "origurl*personal", "origurl*express-scripts.com",
"origurl*madamecleo", "origurl*malibu", "origurl*sjsu.edu", "origurl*news",
"origurl*www.ems.psu.edu", "origurl*7735+mcp", "origurl*sox+htm",
"origurl*mcet.edu", "origurl*punkie+htm", "origurl*midnite",
"origurl*foxy.net", "origurl*maple.lemoyne.edu", "origurl*times",
"origurl*news.com", "origurl*4010+index", "origurl*www.slake.com",
"origurl*larx.com", "origurl*www.FlowSoft.com", "origurl*paper",
"origurl*members+keith", "origurl*paper+1998", "origurl*inwap.com",
"origurl*www.tkz.fh-rpl.de", "origurl*members.accessus.net",
"origurl*kattsida+html", "origurl*cjackson+kandinsky", "origurl*main+htm",
"origurl*www.mei.co.jp", "origurl*www.adamspharmacy.com", "origurl*20+timeout",
"origurl*corridor+5139", "origurl*falcon.sonic.net", "origurl*lycos.co.uk",
"origurl*ran", "origurl*afn47757+midnite", "origurl*www.vrn.de",
"origurl*5309+index", "origurl*stvkat+htm", "origurl*hpe", "origurl*wv+square",
"origurl*keith", "origurl*www.pacificrim.net", "origurl*how",
"origurl*www.metacrawler.com", "origurl*pacificrim.net", "origurl*5257+simstory",
"origurl*cmeese", "origurl*metacrawler.com", "origurl*catscats",
"origurl*wosc", "origurl*xoom.com", "origurl*innotts.co.uk",
"origurl*paris+3963", "origurl*general+kushmerick", "origurl*mindlink.net",
"origurl*cybermog", "origurl*kerouac+htm", "origurl*internauts.ca",
"origurl*mcp", "origurl*martnet.com", "origurl*toyotaofroswell.com",
"origurl*msheryl", "origurl*vault", "origurl*malibu+html", "origurl*jsd+how",
"origurl*polypkem+index", "origurl*corridor", "origurl*zoe+html",
"origurl*afn.org", "origurl*theinternetadvantage.com", "origurl*coltrane+htm",
"origurl*timeout", "origurl*8078+home", "origurl*turbo+html",
"origurl*heartland+meadows", "origurl*news-observer.com", "origurl*www.psnw.com",
"origurl*contents+html", "origurl*tkaine+mogwhi", "origurl*stanford.edu",
"origurl*www.bossintl.com", "origurl*general", "origurl*switchboard.com",
"origurl*cgidir", "origurl*access.ch", "origurl*sydkat+htm",
"origurl*hotsprings", "origurl*baons", "origurl*peace", "origurl*2647+chopin",
"origurl*www.worlddrugstore.com", "origurl*actup+index", "origurl*bin+cgidir",
"origurl*www.internauts.ca", "origurl*library+pitcairn", "origurl*inwap+cats",
"origurl*lolnad", "origurl*pterry", "origurl*lovisa1", "origurl*www.dreamn.com",
"origurl*polypkem", "origurl*tm.fr", "origurl*athens", "origurl*copymat.com",
"origurl*soho", "origurl*victoria", "origurl*bossintl.com", "origurl*westhollywood",
"origurl*private", "origurl*cat", "origurl*cats+htm", "origurl*malek+kandin",
"origurl*yosemite", "origurl*www.inwap.com", "origurl*afn47757+plato",
"origurl*timstory", "origurl*excite.com", "origurl*forum+8078",
"origurl*cats", "origurl*kitty+and", "origurl*dreamn.com", "origurl*irish+times",
"origurl*crawler+general", "origurl*bordeaux+actup", "origurl*hotsprings+4527",
"origurl*kbell", "origurl*stinky+cats", "origurl*plains", "origurl*netscape.com",
"origurl*csuhayward.edu", "origurl*safehaven", "origurl*tripod.com",
"origurl*alley+6750", "origurl*sanjosesabercats.com", "origurl*www.news.com",
"origurl*mogwhi+htm", "origurl*www.theinternetadvantage.com",
"origurl*thejeep.com", "origurl*3963+leonora", "origurl*www.wednet.com",
"origurl*autopen.com", "origurl*charlie+html", "origurl*forum",
"origurl*kaitlin", "origurl*april", "origurl*grouper.com", "origurl*www.ran.org",
"origurl*sydkat", "origurl*heartland+6712", "origurl*rpp+20",
"origurl*2647+kaitlin", "origurl*psychref+html", "origurl*josefina3+horn",
"origurl*malek", "origurl*stvkat", "origurl*timstory+html", "origurl*corp",
"origurl*story", "origurl*thriveonline.com", "origurl*bds+punkie",
"origurl*zoe", "origurl*kerouac", "origurl*123greetings.com",
"origurl*2647+kerouac", "origurl*www.access.ch", "origurl*www.express-scripts.com",
"origurl*paws", "origurl*wednet.com", "origurl*francois", "origurl*zdnet.com",
"origurl*simstory", "origurl*www.puc.edu", "origurl*www.irish-times.com",
"origurl*www.copymat.com", "origurl*cjackson", "origurl*kaitlin+htm",
"origurl*tori+html", "origurl*southbeach+7597", "origurl*home+html",
"origurl*hpe+10", "origurl*pad+htm", "origurl*psu.edu", "origurl*www.shareware.com",
"origurl*bin", "origurl*arvann+pages", "origurl*tori", "origurl*dumble",
"origurl*www.icgnet.com", "origurl*romancebooks", "origurl*3727+turbo",
"origurl*football", "origurl*msheryl+djang", "origurl*zueri.ch",
"origurl*aol.com", "origurl*bds", "origurl*cat+html", "origurl*and+tori",
"origurl*hollywood+9662", "origurl*djang+html", "origurl*yahoo.co.uk",
"origurl*truluck.com", "origurl*shtml", "origurl*link+chuck",
"origurl*members", "origurl*www.thejeep.com", "origurl*2meta.com",
"origurl*cats+html", "origurl*www.autopen.com", "origurl*hevern",
"origurl*ikenobo", "origurl*accessus.net", "origurl*www.wco.com",
"origurl*contents", "origurl*pitcairn", "origurl*westhollywood+4010",
"origurl*messier+html", "origurl*icgnet.com", "origurl*mandypaul+main",
"origurl*links+carousel", "origurl*square+chapel", "origurl*www.grouper.com",
"origurl*6712+cats", "origurl*www.MeissnerChevrolet.autotown.com",
"origurl*globec.com.au", "origurl*meadows", "origurl*afn47757",
"origurl*www.geocities.com", "origurl*haestad.com", "origurl*midnite+htm",
"origurl*tkaine+sox", "origurl*lovisa1+kattsida", "origurl*fr+bordeaux",
"origurl*mindspring.com", "origurl*7597+kitty", "origurl*wired.com",
"origurl*paws+pad", "origurl*irish", "origurl*square", "origurl*pad",
"origurl*method", "origurl*www.mcs.csuhayward.edu", "origurl*www.pacific.net.sg",
"origurl*chuck", "origurl*ran.org", "origurl*misfits2+htm", "origurl*stinky",
"origurl*horn", "origurl*heartland+valley", "origurl*heartland+hills",
"origurl*www.finest.tm.fr", "origurl*carousel+links", "origurl*kushmerick+method",
"origurl*txt", "origurl*method+0", "origurl*title", "origurl*fbox.vt.edu",
"origurl*index+html", "origurl*asp", "origurl*mcp+htm", "origurl*home",
"origurl*netcenter", "origurl*hydrogeologist", "origurl*2539+index",
"origurl*teams", "origurl*\302\256ion", "ancurl*search+direct",
"ancurl*likesbooks.com", "ancurl*mirror", "ancurl*www.slake.com",
"ancurl*cnet+cat", "ancurl*hydrogeologist", "ancurl*geoguide",
"ancurl*2fcrawler", "ancurl*clawring+htm", "ancurl*tkaine+kats",
"ancurl*labyrinth", "ancurl*clickthru+clickid", "ancurl*redirect",
"ancurl*com+ads", "ancurl*area51+corridor", "ancurl*express-scripts.com",
"ancurl*cnet", "ancurl*time+1998", "ancurl*josefina3", "ancurl*truluck.com",
"ancurl*cgiadman", "ancurl*linkpg+htm", "ancurl*geoguide+rank",
"ancurl*area51", "ancurl*www.internauts.ca", "ancurl*afn.org",
"ancurl*www.microsoft.com", "ancurl*nph", "ancurl*ran.org", "ancurl*comprod+mirror",
"ancurl*shareware.com", "ancurl*jump", "ancurl*area51+labyrinth",
"ancurl*pics", "ancurl*nsads.hotwired.com", "ancurl*new", "ancurl*net",
"ancurl*relocate+ad", "ancurl*sendform", "ancurl*carousel.org",
"ancurl*members.accessus.net", "ancurl*home+htm", "ancurl*index+html",
"ancurl*www.FlowSoft.com", "ancurl*kandinsky", "ancurl*csuhayward.edu",
"ancurl*www.yahoo.co.uk", "ancurl*ibitexas.com", "ancurl*www.monmouth.com",
"ancurl*infoserver.etl.vt.edu", "ancurl*heartland+pointe", "ancurl*msn",
"ancurl*468x60", "ancurl*bin+click", "ancurl*general", "ancurl*keith+dumble",
"ancurl*cqi.com", "ancurl*heartland+plains", "ancurl*redir",
"ancurl*webring.org", "ancurl*uk+sv", "ancurl*mem", "ancurl*next",
"ancurl*pacific.net.sg", "ancurl*monmouth.com", "ancurl*news",
"ancurl*adclick", "ancurl*default", "ancurl*exe+cid", "ancurl*claw",
"ancurl*bin+webring", "ancurl*emailbox", "ancurl*sunsite.unc.edu",
"ancurl*exec+obidos", "ancurl*obidos", "ancurl*www.pacific.net.sg",
"ancurl*library", "ancurl*puc.edu", "ancurl*internauts.ca", "ancurl*n+a",
"ancurl*www.ibitexas.com", "ancurl*ucsd.edu", "ancurl*kats",
"ancurl*cjackson+kandinsky", "ancurl*clickid", "ancurl*any+time",
"ancurl*1+cid", "ancurl*ring", "ancurl*FlowSoft.com", "ancurl*nph+adclick",
"ancurl*adclick+exe", "ancurl*event", "ancurl*general+site",
"ancurl*2fclick", "ancurl*home.netscape.com", "ancurl*bin+homestead",
"ancurl*click+runid", "ancurl*martnet", "ancurl*doubleclick.net",
"ancurl*st+uk", "ancurl*catring+html", "ancurl*ng+spacedesc",
"ancurl*familyid", "ancurl*yahoo", "ancurl*site", "ancurl*redirect+dll",
"ancurl*pointe+1154", "ancurl*home+html", "ancurl*www.123greetings.com",
"ancurl*cat+general", "ancurl*www.inwap.com", "ancurl*cjackson",
"ancurl*labyrinth+9439", "ancurl*library+pitcairn", "ancurl*www.mei.co.jp",
"ancurl*april", "ancurl*comprod", "ancurl*ukie", "ancurl*home",
"ancurl*www.lycos.co.uk", "ancurl*jsd", "ancurl*ans+entry", "ancurl*keith",
"ancurl*www.icgnet.com", "ancurl*forum", "ancurl*redirect+cgi",
"ancurl*www.afn.org", "ancurl*geoguide+emailme", "ancurl*time",
"ancurl*www.ran.org", "ancurl*main", "ancurl*members+keith",
"ancurl*http+ad", "ancurl*2f+2fwww", "ancurl*ad+bin", "ancurl*www.amazon.com",
"ancurl*4+mem", "ancurl*www.truluck.com", "ancurl*bridalinfo",
"ancurl*pics+cats", "ancurl*direct", "ancurl*image+http", "ancurl*fbox.vt.edu",
"ancurl*unc.edu", "ancurl*d+ukie", "ancurl*forums", "ancurl*co+uk",
"ancurl*e+html", "ancurl*inwap+cats", "ancurl*malek", "ancurl*jsd+how",
"ancurl*www.cqi.com", "ancurl*redirect+http", "ancurl*name",
"ancurl*1+d", "ancurl*kat001", "ancurl*magic+adclick", "ancurl*grouper.com",
"ancurl*readersndex+com", "ancurl*excite+468x60", "ancurl*click+ng",
"ancurl*www.martnet.com", "ancurl*http+2f", "ancurl*direct+topic",
"ancurl*how", "ancurl*www.wednet.com", "ancurl*www.globec.com.au",
"ancurl*athens", "ancurl*info", "ancurl*schwab", "ancurl*sjsu.edu",
"ancurl*www.puc.edu", "ancurl*a+uk", "ancurl*www.news.com", "ancurl*2fclick+ng",
"ancurl*ads.msn.com", "ancurl*images", "ancurl*access.ch", "ancurl*home+ring",
"ancurl*plug.go2net.com", "ancurl*geocities.com", "ancurl*geoguide+tour",
"ancurl*copymat.com", "ancurl*www.webring.org", "ancurl*groupid+1",
"ancurl*gif+pk", "ancurl*dll+cid", "ancurl*adid", "ancurl*vt.edu",
"ancurl*worlddrug.com", "ancurl*edition", "ancurl*www.express-scripts.com",
"ancurl*personal", "ancurl*gid", "ancurl*bin+geoguide", "ancurl*mindlink.net",
"ancurl*accessus.net", "ancurl*type", "ancurl*ng+type", "ancurl*polypkem",
"ancurl*fools", "ancurl*bin+cgiadman", "ancurl*marketplace",
"ancurl*wco.com", "ancurl*links+html", "ancurl*tkaine+bars",
"ancurl*icgnet.com", "ancurl*www.autotown.com", "ancurl*adcount+plx",
"ancurl*1+familyid", "ancurl*yahoo.com", "ancurl*page", "ancurl*theinternetadvantage.com",
"ancurl*runofsite", "ancurl*romancebooks", "ancurl*francois",
"ancurl*links", "ancurl*athens+forum", "ancurl*site+sw", "ancurl*autotown.com",
"ancurl*amazon.com", "ancurl*site+ne", "ancurl*qual", "ancurl*members",
"ancurl*exe", "ancurl*pratchett", "ancurl*afn47757", "ancurl*esi",
"ancurl*1154+linkpg", "ancurl*desc", "ancurl*forum+8078", "ancurl*topic",
"ancurl*bin+acc", "ancurl*valley", "ancurl*delv", "ancurl*family",
"ancurl*lspace.org", "ancurl*www.pacificrim.net", "ancurl*about",
"ancurl*webring+ring", "ancurl*www.likesbooks.com", "ancurl*bossintl.com",
"ancurl*runid", "ancurl*wednet.com", "ancurl*news.com", "ancurl*lycos.co.uk",
"ancurl*arvann+images", "ancurl*526+redirect", "ancurl*ads+msn",
"ancurl*netscape.com", "ancurl*athens+8774", "ancurl*click+profileid",
"ancurl*qual+mc", "ancurl*homestead+ans", "ancurl*paul.spu.edu",
"ancurl*dll", "ancurl*readersndex", "ancurl*dir", "ancurl*rank",
"ancurl*2meta.com", "ancurl*Lpage.com", "ancurl*emailme", "ancurl*event+ng",
"ancurl*3a+2fcrawler", "ancurl*cgiadman+dll", "ancurl*aol.com",
"ancurl*lg+gif", "ancurl*magic", "ancurl*bin+ad", "ancurl*www.worlddrug.com",
"ancurl*ring+midnight", "ancurl*irish-times.com", "ancurl*download",
"ancurl*http+www", "ancurl*homepage", "ancurl*midnight+id", "ancurl*ad.doubleclick.net",
"ancurl*exec", "ancurl*mem+1", "ancurl*msn+com", "ancurl*preferences",
"ancurl*contact", "ancurl*bars", "ancurl*products", "ancurl*www.geocities.com",
"ancurl*www.carousel.org", "ancurl*spu.edu", "ancurl*com", "ancurl*cnm",
"ancurl*webring+home", "ancurl*link+pics", "ancurl*cid", "ancurl*psu.edu",
"ancurl*ads+redirect", "ancurl*cfm", "ancurl*www.grouper.com",
"ancurl*member", "ancurl*index", "ancurl*cat", "ancurl*www.dreamn.com",
"ancurl*afn47757+clawring", "ancurl*switchboard.com", "ancurl*ad+preferences",
"ancurl*inwap", "ancurl*adcount", "ancurl*martnet.com", "ancurl*thejeep.com",
"ancurl*search", "ancurl*shtml", "ancurl*www+yahoo", "ancurl*desc+4",
"ancurl*cats", "ancurl*pitcairn", "ancurl*www.access.ch", "ancurl*runofsite+any",
"ancurl*url", "ancurl*dejay", "ancurl*gallery", "ancurl*bishop",
"ancurl*features+forums", "ancurl*area", "ancurl*www.mcs.csuhayward.edu",
"ancurl*bin", "ancurl*bds", "ancurl*www.wco.com", "ancurl*www.toyotaofroswell.com",
"ancurl*www.copymat.com", "ancurl*go2net.com", "ancurl*delv+dir",
"ancurl*slake.com", "ancurl*www+readersndex", "ancurl*www.theinternetadvantage.com",
"ancurl*edition+cnet", "ancurl*tkaine", "ancurl*catring", "ancurl*asp",
"ancurl*art", "ancurl*relocate", "ancurl*468x60+runofsite", "ancurl*valley+2647",
"ancurl*any", "ancurl*ans", "ancurl*corridor", "ancurl*amp",
"ancurl*top", "ancurl*pacificrim.net", "ancurl*image", "ancurl*heartland+valley",
"ancurl*tii", "ancurl*123greetings.com", "ancurl*ads", "ancurl*www.yahoo.com",
"ancurl*cuteftp", "ancurl*acc", "ancurl*bin+search", "ancurl*www.shareware.com",
"ancurl*dir+qual", "ancurl*globec.com.au", "ancurl*www.news-observer.com",
"ancurl*index+htm", "ancurl*xmission.com", "ancurl*ukie+s", "ancurl*629+redirect",
"ancurl*pointe", "ancurl*www.ems.psu.edu", "ancurl*http+ads",
"ancurl*members.aol.com", "ancurl*www.xmission.com", "ancurl*yahoo.co.uk",
"ancurl*ring+kat001", "ancurl*msn.com", "ancurl*groupid", "ancurl*geoguide+geoguide",
"ancurl*sanjosesabercats.com", "ancurl*excite", "ancurl*april+fools",
"ancurl*slagen", "ancurl*link", "ancurl*tour+member", "ancurl*uk",
"ancurl*people", "ancurl*sw", "ancurl*sv", "ancurl*st", "ancurl*sj",
"ancurl*pl", "ancurl*pk", "ancurl*www.bossintl.com", "ancurl*ng",
"ancurl*ne", "ancurl*mc", "ancurl*lg", "ancurl*spacedesc", "ancurl*it",
"ancurl*ie", "ancurl*id", "ancurl*go", "ancurl*gg", "ancurl*homestead",
"ancurl*bin+nph", "ancurl*peace", "ancurl*t+s", "ancurl*de",
"ancurl*co", "ancurl*dreamn.com", "ancurl*plx+http", "ancurl*ad",
"ancurl*rnd", "ancurl*dumble", "ancurl*kandinsky+p", "ancurl*linkpg",
"ancurl*webring", "ancurl*ref", "ancurl*www.2meta.com", "ancurl*ran",
"ancurl*ukonline.co.uk", "ancurl*www.Lpage.com", "ancurl*2fwww",
"ancurl*ring+claw", "ancurl*midnight", "ancurl*hotwired.com",
"ancurl*how+htm", "ancurl*tm.fr", "ancurl*ora.com", "ancurl*marketing",
"ancurl*toyotaofroswell.com", "ancurl*heartland", "ancurl*s+b",
"ancurl*mcet.edu", "ancurl*clickthru", "ancurl*enchantedforest",
"ancurl*clawring", "ancurl*news-observer.com", "ancurl*plains",
"ancurl*entry", "ancurl*acc+clickthru", "ancurl*3a", "ancurl*www.irish-times.com",
"ancurl*2f", "ancurl*dll+type", "ancurl*soho", "ancurl*features",
"ancurl*adclick+id", "ancurl*click", "ancurl*preferences+com",
"ancurl*claw+id", "ancurl*url+http", "ancurl*microsoft.com",
"ancurl*excite.com", "ancurl*web.ukonline.co.uk", "ancurl*gort.ucsd.edu",
"ancurl*com+home", "ancurl*2fcgi", "ancurl*pub", "ancurl*bin+adcount",
"ancurl*mc+3a", "ancurl*phoenix.mcet.edu", "ancurl*plx", "ancurl*inwap.com",
"ancurl*www.cob.sjsu.edu", "ancurl*www.thejeep.com", "ancurl*tour",
"ancurl*www.sanjosesabercats.com", "ancurl*mei.co.jp", "ancurl*geoguide+sendform",
"ancurl*services", "ancurl*type+click", "ancurl*tagvalues", "ancurl*profileid",
"ancurl*zueri.ch", "ancurl*arvann", "ancurl*tagvalues+13", "alt*your",
"alt*and", "alt*top", "alt*all", "alt*email", "alt*out", "alt*our",
"alt*here", "alt*the", "alt*download", "alt*netscape", "alt*what",
"alt*explorer", "alt*more", "alt*for+a", "alt*take+a", "alt*kitty",
"alt*visit+our", "alt*link", "alt*wacky", "alt*like", "alt*heartland",
"alt*visit+the", "alt*romance", "alt*online", "alt*products",
"alt*now", "alt*click+here", "alt*pages+like", "alt*rank", "alt*information",
"alt*us", "alt*send+this", "alt*up", "alt*to", "alt*chat", "alt*new",
"alt*net", "alt*my+site", "alt*search", "alt*this", "alt*on",
"alt*of", "alt*links", "alt*bytes", "alt*my", "alt*me", "alt*picture",
"alt*club", "alt*pages", "alt*this+page", "alt*in", "alt*on+the",
"alt*guestbook", "alt*by", "alt*click", "alt*at", "alt*ad", "alt*mine",
"alt*amazon+com", "alt*web", "alt*next", "alt*news", "alt*info",
"alt*join", "alt*of+the", "alt*book", "alt*amazon", "alt*forums",
"alt*back", "alt*next+site", "alt*a+tour", "alt*com", "alt*microsoft",
"alt*geocities", "alt*ring", "alt*cat", "alt*send", "alt*internet+explorer",
"alt*banner", "alt*home+page", "alt*internet", "alt*find", "alt*cool",
"alt*my+guestbook", "alt*here+for", "alt*with", "alt*get", "alt*take",
"alt*tour", "alt*site", "alt*to+visit", "alt*rank+my", "alt*from",
"alt*page", "alt*graphic", "alt*like+mine", "alt*email+me", "alt*visit",
"alt*free", "alt*the+kat", "alt*award", "alt*services", "alt*about",
"alt*for", "alt*here+to", "alt*network", "alt*you", "alt*logo",
"alt*home", "alt*kat", "caption*and", "caption*home+page", "caption*click+here",
"caption*the", "caption*pratchett", "caption*here+for", "caption*site",
"caption*page", "caption*to", "caption*of", "caption*home", "caption*my",
"caption*your", "caption*in", "caption*bytes", "caption*here",
"caption*click", "caption*for", "caption*you"), type = c("numeric",
"numeric", "numeric", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary", "binary", "binary", "binary",
"binary", "binary", "binary", "binary")), class = "data.frame", row.names = c(NA,
-1558L))


# BEGIN EMBEDDED SOURCE: R/utils.R
.submission_modules[["R/utils.R"]] <- quote({
# =============================================================================
# utils.R -- Shared constants and helpers (I/O, LaTeX tables, plotting theme)
# Project : Statistical Analysis and Comparative Evaluation of Internet
#           Advertisement Prediction Models (MT2013)
# Owner   : Quang (P1)
#
# The runner creates and selects results/ before executing the analysis blocks.
# =============================================================================

PROJECT_SEED <- 42L
NUMERIC_VARS <- c("height", "width", "aratio")
BINARY_GROUPS <- c("url", "origurl", "ancurl", "alt", "caption")
# Group sizes documented in ad.names (UCI "STANDARD encoding")
GROUP_SIZES <- c(url = 457L, origurl = 495L, ancurl = 472L, alt = 111L, caption = 19L)

# Colours (validated categorical slots 1-2): nonad = blue, ad = orange
CLASS_COLOURS <- c(nonad = "#2a78d6", ad = "#eb6834")

PATHS <- list(
  raw_dir     = "data/raw",
  processed   = "data/processed",
  out_fig     = "outputs/figures",
  out_tab     = "outputs/tables",
  report_fig  = "report/figures",
  report_tab  = "report/tables"
)

check_project_root <- function() {
  if (!dir.exists(PATHS$raw_dir)) {
    stop("Working directory phai la thu muc goc cua project (chua R/ va data/raw/). ",
         "Hay mo ad_project.Rproj trong RStudio hoac dung setwd().", call. = FALSE)
  }
  for (p in PATHS) dir.create(p, recursive = TRUE, showWarnings = FALSE)
  invisible(TRUE)
}

# Reproducible RNG: same results on every machine with R >= 3.6
set_project_seed <- function(seed = PROJECT_SEED) {
  set.seed(seed, kind = "Mersenne-Twister", normal.kind = "Inversion",
           sample.kind = "Rejection")
}

# ---------------------------------------------------------------------------
# Feature dictionary: maps short, syntactic column names (url_001, ...) to the
# original terms in ad.names (e.g. "url*images+buttons").
# ---------------------------------------------------------------------------
build_feature_dictionary <- function(names_file = file.path(PATHS$raw_dir, "ad.names")) {
  terms <- NULL
  if (file.exists(names_file)) {
    lines <- readLines(names_file, warn = FALSE)
    lines <- iconv(lines, from = "latin1", to = "UTF-8")   # file contains latin1 bytes
    hit <- grepl("^(url|origurl|ancurl|alt|caption)\\*.*: 0,1\\.\\s*$", lines)
    terms <- sub(": 0,1\\.\\s*$", "", lines[hit])
    if (length(terms) != sum(GROUP_SIZES)) terms <- NULL
  }
  groups <- rep(names(GROUP_SIZES), GROUP_SIZES)
  idx <- unlist(lapply(GROUP_SIZES, seq_len), use.names = FALSE)
  if (is.null(terms)) terms <- paste0(groups, "*term", idx)   # fallback if ad.names absent
  bin <- data.frame(
    column        = sprintf("%s_%03d", groups, idx),
    group         = groups,
    original_name = terms,
    type          = "binary",
    stringsAsFactors = FALSE
  )
  head4 <- data.frame(
    column        = c(NUMERIC_VARS, "local"),
    group         = c(rep("geometry", 3), "local"),
    original_name = c(NUMERIC_VARS, "local"),
    type          = c(rep("numeric", 3), "binary"),
    stringsAsFactors = FALSE
  )
  rbind(head4, bin)
}

# ---------------------------------------------------------------------------
# Read the raw file. Supports both the UCI file (ad.data, no header) and the
# Kaggle file (add.csv, header + index column). Missing values are coded "?"
# with surrounding blanks, so everything is read as character first.
# ---------------------------------------------------------------------------
read_ad_raw <- function(raw_dir = PATHS$raw_dir) {
  uci    <- file.path(raw_dir, "ad.data")
  kaggle <- file.path(raw_dir, "add.csv")
  if (file.exists(uci)) {
    raw <- read.csv(uci, header = FALSE, colClasses = "character",
                    strip.white = TRUE, na.strings = character(0))
    source_name <- "UCI ad.data"
  } else if (file.exists(kaggle)) {
    raw <- read.csv(kaggle, header = TRUE, colClasses = "character",
                    strip.white = TRUE, na.strings = character(0),
                    check.names = FALSE)
    if (ncol(raw) == 1560) raw <- raw[, -1, drop = FALSE]   # drop Kaggle row index
    source_name <- "Kaggle add.csv"
  } else {
    stop("Khong tim thay data/raw/ad.data hoac data/raw/add.csv", call. = FALSE)
  }
  if (ncol(raw) != 1559) stop("Expected 1559 columns, found ", ncol(raw), call. = FALSE)

  dict <- build_feature_dictionary(file.path(raw_dir, "ad.names"))
  features <- raw[, 1:1558]
  label    <- trimws(raw[[1559]])

  to_num <- function(v) {
    v <- trimws(v)
    v[v %in% c("?", "")] <- NA
    suppressWarnings(as.numeric(v))
  }
  X <- as.data.frame(lapply(features, to_num))
  names(X) <- dict$column

  list(
    data = data.frame(obs_id = seq_len(nrow(raw)), X,
                      label_raw = label, stringsAsFactors = FALSE),
    dictionary = dict,
    source = source_name,
    # Raw tokens that could not be parsed as numbers and were not "?"
    unparsed = sum(vapply(seq_along(features), function(j) {
      v <- trimws(features[[j]])
      sum(!(v %in% c("?", "")) & is.na(suppressWarnings(as.numeric(v))))
    }, numeric(1)))
  )
}

feature_columns <- function(df) setdiff(names(df), c("obs_id", "y", "label_raw"))
binary_columns  <- function(df) setdiff(feature_columns(df), NUMERIC_VARS)

as_class_factor <- function(y) factor(y, levels = c(0, 1), labels = c("nonad", "ad"))

# ---------------------------------------------------------------------------
# Output helpers
# ---------------------------------------------------------------------------
latex_escape <- function(x) {
  x <- as.character(x)
  x <- gsub("\\\\", "\\\\textbackslash{}", x)
  x <- gsub("([&%$#_{}])", "\\\\\\1", x)
  x <- gsub("~", "\\\\textasciitilde{}", x)
  x <- gsub("\\^", "\\\\textasciicircum{}", x)
  x <- gsub("<", "\\\\textless{}", x)
  x <- gsub(">", "\\\\textgreater{}", x)
  x <- gsub("\\*", "$\\\\ast$", x)
  x
}

fmt_num <- function(x, digits = 2) {
  ifelse(is.na(x), "--", formatC(x, format = "f", digits = digits, big.mark = ","))
}

# Write a data.frame as CSV (outputs/tables) and as a booktabs LaTeX table
# (report/tables) that can be \input{} into the Overleaf report.
save_table <- function(df, name, caption, label = paste0("tab:", name),
                       align = NULL, escape = TRUE, note = NULL,
                       size = "\\small", fit_width = FALSE) {
  write.csv(df, file.path(PATHS$out_tab, paste0(name, ".csv")), row.names = FALSE)
  if (is.null(align)) {
    align <- paste0("l", strrep("r", ncol(df) - 1))
  }
  cells <- as.data.frame(lapply(df, function(col) {
    if (is.numeric(col)) {
      if (all(is.na(col) | col == round(col))) fmt_num(col, 0) else fmt_num(col, 2)
    } else as.character(col)
  }), stringsAsFactors = FALSE)
  header <- names(df)
  if (escape) {
    cells[] <- lapply(cells, latex_escape)
    header <- latex_escape(header)
  }
  body <- apply(cells, 1, paste, collapse = " & ")
  tex <- c(
    "\\begin{table}[htbp]", "\\centering", size,
    paste0("\\caption{", caption, "}"), paste0("\\label{", label, "}"),
    if (fit_width) "\\resizebox{\\textwidth}{!}{%",
    paste0("\\begin{tabular}{", align, "}"), "\\toprule",
    paste0(paste(header, collapse = " & "), " \\\\"), "\\midrule",
    paste0(body, " \\\\"), "\\bottomrule", "\\end{tabular}",
    if (fit_width) "}",
    if (!is.null(note)) paste0("\\par\\vspace{2pt}{\\footnotesize ", note, "}"),
    "\\end{table}"
  )
  writeLines(tex, file.path(PATHS$report_tab, paste0(name, ".tex")))
  invisible(df)
}

has_ggplot <- function() requireNamespace("ggplot2", quietly = TRUE)

theme_report <- function(base_size = 10) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      panel.grid.minor   = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(colour = "#e6e5e1", linewidth = 0.3),
      axis.text          = ggplot2::element_text(colour = "#52514e"),
      axis.title         = ggplot2::element_text(colour = "#0b0b0b"),
      plot.title         = ggplot2::element_text(face = "bold", colour = "#0b0b0b"),
      legend.position    = "top",
      legend.title       = ggplot2::element_blank()
    )
}

# Save a ggplot as PDF (for LaTeX) and PNG (for quick viewing / slides)
save_figure <- function(plot, name, width = 6, height = 3.6) {
  ggplot2::ggsave(file.path(PATHS$report_fig, paste0(name, ".pdf")), plot,
                  width = width, height = height, device = "pdf")
  ggplot2::ggsave(file.path(PATHS$out_fig, paste0(name, ".png")), plot,
                  width = width, height = height, dpi = 200)
  invisible(plot)
}
})
# END EMBEDDED SOURCE: R/utils.R

# BEGIN EMBEDDED SOURCE: R/preprocessing_pipeline.R
.submission_modules[["R/preprocessing_pipeline.R"]] <- quote({
# =============================================================================
# preprocessing_pipeline.R -- Shared, leakage-safe preprocessing for all models
# Owner: Quang (P1).  Used by: Thang (Logistic), Mai (Random Forest), Tai (XGBoost)
#
# Core idea (fit / apply, like recipes::prep / bake):
#   prep <- prep_fit(train_part, prep_options("logistic"))   # learn on training data ONLY
#   tr   <- prep_apply(prep, train_part)                     # transform training data
#   te   <- prep_apply(prep, test_part)                      # same transformation on new data
#
# Every learned quantity (medians, modes, kept features, centring/scaling,
# optional Fisher filter) comes from the data passed to prep_fit(). Inside
# cross-validation, call prep_fit() on the analysis part of each fold
# (see cv_fold_data()). The test set is never used to learn anything.
# =============================================================================

if (!exists("NUMERIC_VARS")) .submission_source("R/utils.R")

# -----------------------------------------------------------------------------
# Options. One preset per model; the feature-filtering rules are identical for
# all three models so that the comparison is fair. Only the model-specific
# steps (log/standardise for LR, native NA handling for XGBoost) differ.
# -----------------------------------------------------------------------------
prep_options <- function(model = c("logistic", "random_forest", "xgboost"), ...) {
  model <- match.arg(model)
  base <- list(
    model             = model,
    numeric_vars      = NUMERIC_VARS,  # continuous predictors to keep
    impute_numeric    = TRUE,          # training-median imputation of height/width/aratio
    impute_local      = TRUE,          # training-mode imputation of 'local' (few NAs)
    missing_indicator = TRUE,          # add geom_missing = 1 if any geometry value is NA
    log_transform     = FALSE,         # log() of numeric vars (strong right skew)
    standardize       = FALSE,         # centre/scale numeric vars with training mean/sd
    min_count         = 5L,            # drop binary feature if its minority value occurs < 5 times in training
    drop_duplicate_columns = TRUE,     # drop binary columns identical to an earlier column in training
    fisher_top_k      = NULL           # optional: keep only the top-k binary features by Fisher p-value
  )
  preset <- switch(model,
    # log(aratio) = log(width) - log(height) exactly -> aratio is redundant for a linear model
    logistic      = list(numeric_vars = c("height", "width"),
                         log_transform = TRUE, standardize = TRUE),
    random_forest = list(),
    # XGBoost learns a default direction for NA, so imputation is not needed
    xgboost       = list(impute_numeric = FALSE, impute_local = FALSE)
  )
  opts <- utils::modifyList(utils::modifyList(base, preset), list(...))
  unknown <- setdiff(names(opts), names(base))
  if (length(unknown)) stop("Unknown option(s): ", paste(unknown, collapse = ", "))
  opts
}

# -----------------------------------------------------------------------------
# prep_fit: learn all preprocessing parameters from (training) data
# -----------------------------------------------------------------------------
prep_fit <- function(train, options = prep_options()) {
  stopifnot(is.data.frame(train), "y" %in% names(train))
  y   <- train$y
  bin <- binary_columns(train)
  num <- options$numeric_vars

  # 1) imputation values (training statistics only)
  medians <- vapply(NUMERIC_VARS, function(v) stats::median(train[[v]], na.rm = TRUE), numeric(1))
  local_mode <- as.numeric(names(which.max(table(train$local))))

  # 2) binary feature filtering
  B <- as.matrix(train[, bin, drop = FALSE])
  B_filled <- B
  B_filled[, "local"][is.na(B_filled[, "local"])] <- local_mode
  ones  <- colSums(B_filled == 1)
  zeros <- nrow(B_filled) - ones
  minority <- pmin(ones, zeros)
  dropped_zero_var <- bin[minority == 0]
  dropped_rare     <- bin[minority > 0 & minority < options$min_count]
  keep <- bin[minority >= options$min_count]

  dropped_dup <- character(0)
  if (isTRUE(options$drop_duplicate_columns) && length(keep) > 1) {
    dup <- duplicated(t(B_filled[, keep, drop = FALSE]))
    dropped_dup <- keep[dup]
    keep <- keep[!dup]
  }

  fisher_p <- NULL
  if (!is.null(options$fisher_top_k) && options$fisher_top_k < length(keep)) {
    fisher_p <- vapply(keep, function(v) {
      stats::fisher.test(table(factor(B_filled[, v], levels = 0:1),
                               factor(y, levels = 0:1)))$p.value
    }, numeric(1))
    keep <- keep[order(fisher_p)][seq_len(options$fisher_top_k)]
    keep <- bin[bin %in% keep]                       # keep original column order
  }

  # 3) numeric transformation parameters (after imputation, on training data)
  numeric_block <- .transform_numeric(train, num, medians, options)
  center <- scale <- NULL
  if (isTRUE(options$standardize)) {
    center <- colMeans(numeric_block, na.rm = TRUE)
    scale  <- apply(numeric_block, 2, stats::sd, na.rm = TRUE)
    scale[scale == 0 | is.na(scale)] <- 1
  }

  structure(list(
    options = options,
    medians = medians,
    local_mode = local_mode,
    numeric_vars = num,
    binary_keep = keep,
    dropped = list(zero_variance = dropped_zero_var, rare = dropped_rare,
                   duplicate = dropped_dup),
    fisher_p = fisher_p,
    center = center, scale = scale,
    n_train = nrow(train),
    class_counts = table(factor(y, levels = 0:1, labels = c("nonad", "ad")))
  ), class = "ad_prep")
}

.transform_numeric <- function(df, num, medians, options) {
  out <- as.data.frame(df[, num, drop = FALSE])
  if (isTRUE(options$impute_numeric)) {
    for (v in num) out[[v]][is.na(out[[v]])] <- medians[[v]]
  }
  if (isTRUE(options$log_transform)) {
    for (v in num) out[[v]] <- log(out[[v]])     # all observed values are > 0
  }
  out
}

# -----------------------------------------------------------------------------
# prep_apply: apply a fitted prep to any data (training fold, validation fold, test)
#   target = "integer" -> y in {0,1};  "factor" -> y in {nonad, ad};  "none" -> no y
# -----------------------------------------------------------------------------
prep_apply <- function(prep, data, target = c("integer", "factor", "none"),
                       keep_id = FALSE) {
  stopifnot(inherits(prep, "ad_prep"))
  target <- match.arg(target)
  opts <- prep$options

  num <- .transform_numeric(data, prep$numeric_vars, prep$medians, opts)
  if (isTRUE(opts$standardize)) {
    for (v in names(num)) num[[v]] <- (num[[v]] - prep$center[[v]]) / prep$scale[[v]]
  }

  out <- num
  if (isTRUE(opts$missing_indicator)) {
    out$geom_missing <- as.integer(rowSums(is.na(data[, NUMERIC_VARS, drop = FALSE])) > 0)
  }
  B <- data[, prep$binary_keep, drop = FALSE]
  if (isTRUE(opts$impute_local) && "local" %in% names(B)) {
    B$local[is.na(B$local)] <- prep$local_mode
  }
  out <- cbind(out, B)

  if (keep_id) out <- cbind(obs_id = data$obs_id, out)
  if (target != "none" && "y" %in% names(data)) {
    out <- cbind(y = if (target == "factor") as_class_factor(data$y) else data$y, out)
  }
  rownames(out) <- NULL
  out
}

# Matrix interface for glmnet / xgboost: list(x = numeric matrix, y = 0/1 vector)
prep_matrix <- function(prep, data) {
  df <- prep_apply(prep, data, target = "none")
  list(x = as.matrix(df), y = data$y)
}

print.ad_prep <- function(x, ...) {
  cat("<ad_prep> model preset:", x$options$model, "\n")
  cat("  fitted on", x$n_train, "rows (nonad =", x$class_counts[["nonad"]],
      ", ad =", x$class_counts[["ad"]], ")\n")
  cat("  numeric vars:", paste(x$numeric_vars, collapse = ", "),
      if (isTRUE(x$options$log_transform)) "[log]" else "",
      if (isTRUE(x$options$standardize)) "[standardised]" else "", "\n")
  cat("  binary features kept:", length(x$binary_keep), "\n")
  cat("  dropped: zero-variance =", length(x$dropped$zero_variance),
      "| rare (<", x$options$min_count, ") =", length(x$dropped$rare),
      "| duplicate columns =", length(x$dropped$duplicate), "\n")
  invisible(x)
}

# -----------------------------------------------------------------------------
# Class imbalance helpers (always compute from the TRAINING part)
# -----------------------------------------------------------------------------
# Balanced class weights: w_c = n / (2 * n_c)  -> named c("0" = ..., "1" = ...)
class_weights <- function(y) {
  tab <- table(factor(y, levels = c(0, 1)))
  w <- length(y) / (2 * as.numeric(tab))
  stats::setNames(w, c("0", "1"))
}
# One weight per observation (e.g. glmnet(weights = ...), glm(weights = ...))
case_weights <- function(y) unname(class_weights(y)[as.character(y)])
# XGBoost: scale_pos_weight = #negative / #positive
scale_pos_weight <- function(y) sum(y == 0) / sum(y == 1)

# -----------------------------------------------------------------------------
# Shared data loaders
# -----------------------------------------------------------------------------
load_shared_data <- function(dir = PATHS$processed) {
  files <- file.path(dir, c("train_raw.rds", "test_raw.rds", "cv_folds.rds"))
  if (!all(file.exists(files))) {
    stop("Chua co train/test/folds. Hay chay R/01_data_exploration.R va R/03_data_split.R truoc.",
         call. = FALSE)
  }
  list(train = readRDS(files[1]), test = readRDS(files[2]), folds = readRDS(files[3]))
}

# Data for fold k of the shared CV, with preprocessing re-fitted inside the fold
cv_fold_data <- function(k, train, folds, options = prep_options(),
                         target = c("integer", "factor")) {
  target <- match.arg(target)
  analysis   <- train[folds$index[[k]], , drop = FALSE]
  assessment <- train[folds$indexOut[[k]], , drop = FALSE]
  prep <- prep_fit(analysis, options)
  list(
    k = k,
    prep = prep,
    analysis   = prep_apply(prep, analysis, target = target),
    assessment = prep_apply(prep, assessment, target = target),
    assessment_obs_id = assessment$obs_id
  )
}
})
# END EMBEDDED SOURCE: R/preprocessing_pipeline.R

# BEGIN EMBEDDED SOURCE: R/statistical_analysis_io.R
.submission_modules[["R/statistical_analysis_io.R"]] <- quote({
# =============================================================================
# statistical_analysis_io.R -- Data Loading and Report Output
#
# Shared input and output for descriptive statistics, estimation and tests.
# Input   : data/processed/ad_clean.rds and feature_dictionary.csv
# Output  : Analysis data, groups, and CSV and PNG export helpers
# =============================================================================

# -----------------------------------------------------------------------------
# 1. Load the shared analysis data
# -----------------------------------------------------------------------------
.submission_source("R/utils.R")

# Use the deduplicated sample before splitting or imputing missing values.
data_path <- file.path(PATHS$processed, "ad_clean.rds")
dictionary_path <- file.path(PATHS$processed, "feature_dictionary.csv")
if (!all(file.exists(c(data_path, dictionary_path)))) {
  stop("Run Rscript R/01_data_exploration.R from the repository root first.",
       call. = FALSE)
}
ads <- readRDS(data_path)
# Mark keyword labels as UTF-8 without converting them to the session locale.
dictionary <- read.csv(dictionary_path, encoding = "UTF-8", stringsAsFactors = FALSE)
columns <- setdiff(names(ads), c("obs_id", "y"))
indices <- match(columns, dictionary$column)
features <- dictionary$original_name[indices]
stopifnot(!anyNA(indices), !anyNA(features), !anyDuplicated(features),
          all(c(NUMERIC_VARS, "local") %in% features),
          "y" %in% names(ads), all(ads$y %in% 0:1))
names(ads)[match(columns, names(ads))] <- features

# -----------------------------------------------------------------------------
# 2. Set up analysis groups
# -----------------------------------------------------------------------------
geometry <- NUMERIC_VARS
keywords <- setdiff(features, c(geometry, "local"))
ads$class <- as_class_factor(ads$y)
ads$ad <- ads$y

# These groups retain feature-specific missing values for the summaries and tests.
groups <- list(all = ads, ad = ads[ads$ad == 1, ], nonad = ads[ads$ad == 0, ])

# -----------------------------------------------------------------------------
# 3. Output paths and CSV export
# -----------------------------------------------------------------------------
output_dir <- function(kind) {
  path <- switch(kind,
                 tables = PATHS$out_tab,
                 figures = PATHS$out_fig,
                 report_figures = file.path("report", "Part_2", "figures"))
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  path
}
write_analysis_csv <- function(x, name) {
  write.csv(x, file.path(output_dir("tables"), paste0(name, ".csv")),
            row.names = FALSE, fileEncoding = "UTF-8")
}
# -----------------------------------------------------------------------------
# 4. Figure output
# -----------------------------------------------------------------------------
save_analysis_figure <- function(name, width, height, draw) {
  png_name <- paste0(name, ".png")
  output_png <- file.path(output_dir("figures"), png_name)
  report_png <- file.path(output_dir("report_figures"), png_name)

  png(output_png, width = width, height = height, units = "in", res = 200)
  tryCatch(draw(), finally = dev.off())
  if (!file.copy(output_png, report_png, overwrite = TRUE)) {
    stop("Could not copy figure to the report: ", png_name, call. = FALSE)
  }
}
})
# END EMBEDDED SOURCE: R/statistical_analysis_io.R

# BEGIN EMBEDDED SOURCE: R/p4_helpers.R
.submission_modules[["R/p4_helpers.R"]] <- quote({
# Shared evaluation/output helpers for Quynh Mai (P4).
# Existing P1/P3 scripts and their reported results are left intact.
.submission_source("R/utils.R")
.submission_source("R/preprocessing_pipeline.R")

p4_init <- function() {
  if (!dir.exists("data/processed")) stop("Run from the project root.")
  for (d in c("outputs/tables", "outputs/figures", "outputs/models",
              "outputs/predictions", "report/Part_4/tables",
              "report/Part_4/figures", "report/Part_4/sections")) {
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
  }
  shared <- load_shared_data()
  stopifnot(!anyDuplicated(shared$train$obs_id),
            !anyDuplicated(shared$test$obs_id),
            length(intersect(shared$train$obs_id, shared$test$obs_id)) == 0L,
            identical(shared$folds$obs_id, shared$train$obs_id))
  shared
}

p4_validate_scores <- function(y, probability) {
  if (!is.numeric(y) || anyNA(y) || !all(y %in% 0:1) || !length(y)) {
    stop("Actual labels must be numeric 0/1 without missing values.")
  }
  if (!is.numeric(probability) || length(y) != length(probability) ||
      any(!is.finite(probability)) || any(probability < 0 | probability > 1)) {
    stop("Probabilities must be finite, in [0,1], and match the labels.")
  }
  invisible(TRUE)
}

# One point per distinct probability: ties never depend on CSV row order.
p4_curves <- function(y, probability) {
  p4_validate_scores(y, probability)
  ord <- order(probability, decreasing = TRUE)
  score <- probability[ord]
  positive <- y[ord] == 1
  ends <- which(!duplicated(score, fromLast = TRUE))
  tp <- cumsum(positive)[ends]
  fp <- cumsum(!positive)[ends]
  n1 <- sum(y == 1)
  n0 <- sum(y == 0)
  data.frame(threshold = c(Inf, score[ends]),
             recall = c(0, if (n1) tp / n1 else rep(NA_real_, length(tp))),
             precision = c(1, tp / (tp + fp)),
             fpr = c(0, if (n0) fp / n0 else rep(NA_real_, length(fp))))
}

p4_metrics <- function(y, probability, predicted = NULL, threshold = 0.5) {
  p4_validate_scores(y, probability)
  if (is.null(predicted)) predicted <- as.integer(probability >= threshold)
  if (length(predicted) != length(y) || anyNA(predicted) ||
      !all(predicted %in% 0:1)) stop("Predicted labels must be 0/1 and match y.")
  tp <- sum(y == 1 & predicted == 1)
  fp <- sum(y == 0 & predicted == 1)
  fn <- sum(y == 1 & predicted == 0)
  tn <- sum(y == 0 & predicted == 0)
  n1 <- tp + fn
  n0 <- tn + fp
  precision <- if (tp + fp) tp / (tp + fp) else 0
  recall <- if (n1) tp / n1 else 0
  f1 <- if (2 * tp + fp + fn) 2 * tp / (2 * tp + fp + fn) else 0
  ranks <- rank(probability, ties.method = "average")
  roc <- if (n1 && n0) (sum(ranks[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0) else NA_real_
  curve <- p4_curves(y, probability)
  pr <- if (n1) sum(diff(curve$recall) *
    (head(curve$precision, -1) + tail(curve$precision, -1)) / 2) else NA_real_
  c(Accuracy = (tp + tn) / length(y), Precision = precision, Recall = recall,
    Specificity = if (n0) tn / n0 else NA_real_, F1 = f1,
    ROC_AUC = roc, PR_AUC = pr, TP = tp, FP = fp, FN = fn, TN = tn)
}

p4_choose_threshold <- function(y, probability) {
  thresholds <- seq(0.10, 0.90, by = 0.01)
  scores <- t(vapply(thresholds, function(th) p4_metrics(y, probability, threshold = th),
                     numeric(11)))
  tab <- data.frame(threshold = thresholds, scores, row.names = NULL)
  tied <- which(abs(tab$F1 - max(tab$F1)) < 1e-12)
  chosen <- tied[order(abs(thresholds[tied] - 0.5), thresholds[tied])][1]
  list(threshold = thresholds[chosen], table = tab)
}

p4_read_predictions <- function(path, test) {
  d <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  if (anyDuplicated(names(d))) stop(path, ": duplicate prediction column names.")
  if (all(c("sample_id", "obs_id") %in% names(d)) &&
      (!identical(as.character(d$sample_id), as.character(d$obs_id)) ||
       anyNA(d$sample_id) || anyNA(d$obs_id))) {
    stop(path, ": sample_id and obs_id aliases disagree.")
  }
  if (!"sample_id" %in% names(d) && "obs_id" %in% names(d)) names(d)[names(d) == "obs_id"] <- "sample_id"
  required <- c("sample_id", "actual", "predicted", "probability_ad")
  if (!all(required %in% names(d))) stop(path, ": missing required prediction columns.")
  d <- d[, required]
  if (anyNA(d$sample_id) || anyDuplicated(d$sample_id) ||
      nrow(d) != nrow(test) || !setequal(d$sample_id, test$obs_id)) {
    stop(path, ": prediction IDs do not match the locked test set.")
  }
  d <- d[match(test$obs_id, d$sample_id), ]
  rownames(d) <- NULL
  if (!identical(as.integer(d$actual), as.integer(test$y))) stop(path, ": actual labels mismatch.")
  p4_metrics(d$actual, d$probability_ad, d$predicted)
  d$sample_id <- test$obs_id
  d$actual <- as.integer(d$actual)
  d$predicted <- as.integer(d$predicted)
  d
}

p4_latex_escape <- function(x) {
  lookup <- c("\\" = "\\textbackslash{}", "&" = "\\&", "%" = "\\%",
              "$" = "\\$", "#" = "\\#", "_" = "\\_", "{" = "\\{", "}" = "\\}",
              "~" = "\\textasciitilde{}", "^" = "\\textasciicircum{}",
              "<" = "\\textless{}", ">" = "\\textgreater{}")
  vapply(as.character(x), function(s) {
    if (is.na(s)) return("--")
    chars <- strsplit(s, "", fixed = TRUE)[[1]]
    paste0(vapply(chars, function(ch) if (ch %in% names(lookup)) lookup[[ch]] else ch,
                  character(1)), collapse = "")
  }, character(1), USE.NAMES = FALSE)
}

p4_write_manifest <- function(df, name) {
  write.csv(df, file.path("outputs/tables", paste0(name, ".csv")), row.names = FALSE, na = "")
  invisible(df)
}

p4_write_table <- function(df, name, caption, note = NULL) {
  p4_write_manifest(df, name)
  display <- as.data.frame(lapply(df, function(col) {
    if (is.numeric(col)) {
      vapply(col, function(v) if (!is.finite(v)) "--" else
        if (abs(v - round(v)) < 1e-10) format(v, scientific = FALSE, trim = TRUE) else
          formatC(v, digits = 4, format = "f"), character(1))
    } else as.character(col)
  }), stringsAsFactors = FALSE)
  display[] <- lapply(display, p4_latex_escape)
  body <- if (nrow(display)) apply(display, 1, paste, collapse = " & ") else character()
  tex <- c("\\begin{table}[htbp]", "\\centering\\small",
           paste0("\\caption{", p4_latex_escape(caption), "}"),
           paste0("\\label{tab:", name, "}"),
           "\\begingroup\\setbox0=\\hbox{%",
           paste0("\\begin{tabular}{", paste0(rep("l", ncol(df)), collapse = ""), "}"),
           "\\toprule", paste0(paste(p4_latex_escape(names(df)), collapse = " & "), " \\\\"),
           "\\midrule", paste0(body, " \\\\"), "\\bottomrule", "\\end{tabular}}",
           "\\ifdim\\wd0>\\linewidth\\resizebox{\\linewidth}{!}{\\usebox0}\\else\\usebox0\\fi\\endgroup",
           if (!is.null(note)) paste0("\\par\\smallskip{\\footnotesize ", p4_latex_escape(note), "}"),
           "\\end{table}")
  writeLines(tex, file.path("report/Part_4/tables", paste0(name, ".tex")), useBytes = TRUE)
  invisible(df)
}

p4_plot <- function(name, draw, width = 7, height = 4.5) {
  render <- function(path, kind) {
    if (kind == "png") grDevices::png(path, width = width, height = height, units = "in", res = 170) else
      grDevices::pdf(path, width = width, height = height, useDingbats = FALSE)
    on.exit(grDevices::dev.off(), add = TRUE)
    graphics::par(mar = c(4.4, 4.4, 2.6, 1.0), las = 1, family = "sans")
    draw()
  }
  render(file.path("outputs/figures", paste0(name, ".png")), "png")
  render(file.path("outputs/figures", paste0(name, ".pdf")), "pdf")
  file.copy(file.path("outputs/figures", paste0(name, ".pdf")),
            file.path("report/Part_4/figures", paste0(name, ".pdf")), overwrite = TRUE)
  invisible(NULL)
}

# Ordinary nonparametric percentile bootstrap of fixed test predictions.
# The same row sample is shared by every model, preserving paired observations.
# This does not include variability caused by re-training the models.
p4_bootstrap <- function(predictions_named_list, B = 2000L, seed = 42L) {
  models <- names(predictions_named_list)
  if (!length(models) || any(!nzchar(models))) stop("Bootstrap requires named models.")
  reference <- predictions_named_list[[1]]
  n <- nrow(reference)
  metrics <- c("Accuracy", "Precision", "Recall", "F1", "ROC_AUC", "PR_AUC")
  for (d in predictions_named_list) {
    if (!identical(d$sample_id, reference$sample_id) || !identical(d$actual, reference$actual)) {
      stop("Align every model by ID and actual label before paired bootstrap.")
    }
    p4_metrics(d$actual, d$probability_ad, d$predicted)
  }
  estimates <- lapply(predictions_named_list, function(d)
    p4_metrics(d$actual, d$probability_ad, d$predicted)[metrics])
  draws <- lapply(models, function(m) matrix(NA_real_, B, length(metrics),
                                           dimnames = list(NULL, metrics)))
  names(draws) <- models
  set_project_seed(seed)
  for (b in seq_len(B)) {
    ix <- sample.int(n, n, replace = TRUE)
    for (m in models) {
      d <- predictions_named_list[[m]]
      draws[[m]][b, ] <- p4_metrics(d$actual[ix], d$probability_ad[ix], d$predicted[ix])[metrics]
    }
  }
  intervals <- do.call(rbind, lapply(models, function(m) {
    qs <- apply(draws[[m]], 2, stats::quantile, probs = c(0.025, 0.975), na.rm = TRUE, names = FALSE)
    data.frame(model = m, metric = metrics, estimate = unname(estimates[[m]]),
               lower = qs[1, ], upper = qs[2, ], row.names = NULL)
  }))
  differences <- data.frame(model_a = character(), model_b = character(), metric = character(),
                            estimate = numeric(), lower = numeric(), upper = numeric(),
                            p_value = numeric(), p_adjusted = numeric())
  if (length(models) >= 2L) {
    pairs <- utils::combn(models, 2, simplify = FALSE)
    differences <- do.call(rbind, lapply(pairs, function(pair) {
      delta <- draws[[pair[1]]] - draws[[pair[2]]]
      estimate <- estimates[[pair[1]]] - estimates[[pair[2]]]
      qs <- apply(delta, 2, stats::quantile, probs = c(0.025, 0.975), na.rm = TRUE, names = FALSE)
      # Approximate two-sided centered-bootstrap null test; CI is primary.
      p <- vapply(seq_along(metrics), function(j) {
        values <- delta[, j]
        values <- values[is.finite(values)]
        (1 + sum(abs(values - estimate[j]) >= abs(estimate[j]) - 1e-12)) / (1 + length(values))
      }, numeric(1))
      data.frame(model_a = pair[1], model_b = pair[2], metric = metrics,
                 estimate = unname(estimate), lower = qs[1, ], upper = qs[2, ],
                 p_value = p, p_adjusted = NA_real_, row.names = NULL)
    }))
    for (metric in metrics) {
      ix <- which(differences$metric == metric)
      differences$p_adjusted[ix] <- stats::p.adjust(differences$p_value[ix], method = "holm")
    }
  }
  list(intervals = intervals, differences = differences, draws = draws,
       B = B, seed = seed, method = "paired ordinary percentile bootstrap; centered null p-values")
}
})
# END EMBEDDED SOURCE: R/p4_helpers.R

# BEGIN EMBEDDED SOURCE: R/p4_rf_functions.R
.submission_modules[["R/p4_rf_functions.R"]] <- quote({
# Reusable Random Forest fitting/prediction used by P4 training and robustness.
# Source R/p4_helpers.R first so that shared preprocessing helpers are available.

p4_rf_fit <- function(data, params, seed, importance = FALSE) {
  if (!requireNamespace("randomForest", quietly = TRUE)) {
    stop("Package 'randomForest' is required. Install it before running P4.",
         call. = FALSE)
  }
  if (!is.data.frame(data) || !"y" %in% names(data)) {
    stop("RF training data must be a data.frame containing y.", call. = FALSE)
  }
  y <- if (is.factor(data$y)) {
    factor(as.character(data$y), levels = c("nonad", "ad"))
  } else {
    as_class_factor(data$y)
  }
  if (anyNA(y) || length(unique(y)) != 2L) {
    stop("RF training labels must contain both nonad/ad classes.", call. = FALSE)
  }
  predictors <- setdiff(names(data), c("y", "obs_id", "sample_id", "label_raw"))
  x <- data[, predictors, drop = FALSE]
  if (!length(predictors) || anyNA(x)) {
    stop("RF predictors must be nonempty and fully imputed.", call. = FALSE)
  }
  effective_mtry <- max(1L, min(as.integer(params$mtry), length(predictors)))
  weights <- class_weights(as.integer(y == "ad"))
  set_project_seed(as.integer(seed))
  model <- randomForest::randomForest(
    x = x, y = y, ntree = as.integer(params$ntree),
    mtry = effective_mtry, nodesize = as.integer(params$nodesize),
    classwt = c(nonad = unname(weights[["0"]]), ad = unname(weights[["1"]])),
    importance = importance, keep.forest = TRUE
  )
  model$p4_predictors <- predictors
  model$p4_effective_mtry <- effective_mtry
  model$p4_seed <- as.integer(seed)
  model
}

p4_rf_probability <- function(model, data) {
  predictors <- model$p4_predictors
  if (is.null(predictors) || !all(predictors %in% names(data))) {
    stop("Prediction data does not match the fitted RF predictors.", call. = FALSE)
  }
  x <- data[, predictors, drop = FALSE]
  if (anyNA(x)) stop("RF prediction data still contains missing values.", call. = FALSE)
  probability <- predict(model, newdata = x, type = "prob")
  if (!"ad" %in% colnames(probability)) {
    stop("RF probability output has no 'ad' class.", call. = FALSE)
  }
  as.numeric(probability[, "ad"])
}

# Forest prefixes share the same fitted trees. This lets the CV grid compare
# 200/400 trees after fitting only the 400-tree forest for each mtry/node pair.
# The full-prefix vote probability is checked against predict(type="prob").
p4_rf_prefix_probabilities <- function(model, data, ntree_values,
                                       verify_max = TRUE) {
  ntree_values <- unique(as.integer(ntree_values))
  if (!length(ntree_values) || anyNA(ntree_values) ||
      any(ntree_values < 1L | ntree_values > model$ntree)) {
    stop("RF prefix sizes must be between 1 and the fitted tree count.", call. = FALSE)
  }
  predictors <- model$p4_predictors
  if (is.null(predictors) || !all(predictors %in% names(data))) {
    stop("Prediction data does not match the fitted RF predictors.", call. = FALSE)
  }
  prediction <- predict(model, newdata = data[, predictors, drop = FALSE],
                        type = "response", predict.all = TRUE)
  votes_ad <- prediction$individual == "ad"
  probabilities <- lapply(ntree_values, function(n) {
    rowMeans(votes_ad[, seq_len(n), drop = FALSE])
  })
  names(probabilities) <- as.character(ntree_values)
  if (isTRUE(verify_max) && model$ntree %in% ntree_values) {
    full_probability <- p4_rf_probability(model, data)
    if (!isTRUE(all.equal(probabilities[[as.character(model$ntree)]],
                          full_probability, tolerance = 1e-14,
                          check.attributes = FALSE))) {
      stop("Full-forest class votes do not match RF probability output.", call. = FALSE)
    }
  }
  probabilities
}

# Cheap validation for the installed randomForest version: a 200-tree fit
# with the same seed must equal the first 200 trees of the 400-tree fit.
# Run explicitly before full training, e.g. p4_rf_prefix_smoke_check().
p4_rf_prefix_smoke_check <- function() {
  set_project_seed(173L)
  y <- factor(rep(c("nonad", "ad"), 40L), levels = c("nonad", "ad"))
  tiny <- data.frame(y = y, x1 = rnorm(80) + as.integer(y == "ad"),
                     x2 = runif(80), x3 = rbinom(80, 1, 0.35))
  params <- list(ntree = 400L, mtry = 2L, nodesize = 1L)
  forest400 <- p4_rf_fit(tiny, params, seed = 42L)
  prefixes <- p4_rf_prefix_probabilities(forest400, tiny, c(200L, 400L))
  params$ntree <- 200L
  forest200 <- p4_rf_fit(tiny, params, seed = 42L)
  difference200 <- max(abs(prefixes[["200"]] - p4_rf_probability(forest200, tiny)))
  difference400 <- max(abs(prefixes[["400"]] - p4_rf_probability(forest400, tiny)))
  if (difference200 > 1e-14 || difference400 > 1e-14) {
    stop("RF tree-prefix smoke check failed; do not use prefix tuning.", call. = FALSE)
  }
  data.frame(check = c("200-tree prefix versus separate 200-tree forest",
                       "400-tree votes versus standard probability"),
              max_absolute_difference = c(difference200, difference400),
              passed = TRUE)
}

# Limit concurrent copies of the fold matrices. The environment variable can
# reduce concurrency (P4_WORKERS=1 selects the same code path without a cluster).
p4_rf_worker_count <- function(n_folds) {
  cores <- parallel::detectCores(logical = TRUE)
  if (is.na(cores)) cores <- 1L
  cap <- min(3L, max(1L, as.integer(cores) - 1L), as.integer(n_folds))
  requested <- suppressWarnings(as.integer(Sys.getenv("P4_WORKERS", "3")))
  if (is.na(requested) || requested < 1L) {
    stop("P4_WORKERS must be a positive integer.", call. = FALSE)
  }
  max(1L, min(cap, requested))
}

p4_rf_make_cluster <- function(workers, project_root = getwd()) {
  cluster <- parallel::makePSOCKcluster(as.integer(workers), outfile = "")
  ready <- FALSE
  on.exit(if (!ready) try(parallel::stopCluster(cluster), silent = TRUE))
  parallel::clusterCall(cluster, function(root, libraries) {
    setwd(root)
    .libPaths(libraries)
    .submission_source("R/p4_helpers.R")
    .submission_source("R/p4_rf_functions.R")
    if (!requireNamespace("randomForest", quietly = TRUE)) {
      stop("randomForest is not available to a CV worker.")
    }
    invisible(NULL)
  }, normalizePath(project_root, winslash = "/", mustWork = TRUE), .libPaths())
  ready <- TRUE
  cluster
}

# A small return object crosses the worker boundary: no fitted forest is sent
# back, and a worker receives just its current fold rather than all fold data.
p4_rf_cv_task <- function(task, config, seed, ntree_values) {
  fit_start <- proc.time()[["elapsed"]]
  fit <- p4_rf_fit(task$analysis, config, seed + task$k - 1L)
  fit_seconds <- proc.time()[["elapsed"]] - fit_start
  prefix_start <- proc.time()[["elapsed"]]
  probabilities <- p4_rf_prefix_probabilities(fit, task$assessment, ntree_values)
  prefix_seconds <- proc.time()[["elapsed"]] - prefix_start
  list(k = task$k, assessment_obs_id = task$assessment_obs_id,
       actual = as.integer(task$assessment$y == "ad"),
       probabilities = probabilities,
       effective_mtry = fit$p4_effective_mtry,
       n_predictors = length(fit$p4_predictors),
       seed = seed + task$k - 1L,
       shared_max_tree_fit_seconds = fit_seconds,
       shared_prefix_prediction_seconds = prefix_seconds)
}

# Explicit smoke check: predictions must be unchanged by load balancing or
# worker count. Forests are deliberately tiny so this is cheap before a full run.
p4_rf_parallel_smoke_check <- function() {
  set_project_seed(173L)
  y <- factor(rep(c("nonad", "ad"), 30L), levels = c("nonad", "ad"))
  tiny <- data.frame(y = y, x1 = rnorm(60) + as.integer(y == "ad"),
                     x2 = runif(60), x3 = rbinom(60, 1, 0.35))
  tasks <- lapply(seq_len(2L), function(k) {
    assessment <- seq.int(k, nrow(tiny), by = 4L)
    list(k = k, analysis = tiny[-assessment, , drop = FALSE],
         assessment = tiny[assessment, , drop = FALSE],
         assessment_obs_id = assessment)
  })
  config <- list(ntree = 40L, mtry = 2L, nodesize = 1L)
  serial <- lapply(tasks, p4_rf_cv_task, config = config,
                    seed = 42L, ntree_values = c(20L, 40L))
  cluster <- p4_rf_make_cluster(2L)
  on.exit(parallel::stopCluster(cluster), add = TRUE)
  concurrent <- parallel::parLapplyLB(cluster, tasks, p4_rf_cv_task,
                                      config = config, seed = 42L,
                                      ntree_values = c(20L, 40L))
  differences <- vapply(seq_along(tasks), function(k) {
    stopifnot(identical(serial[[k]]$assessment_obs_id,
                         concurrent[[k]]$assessment_obs_id))
    max(abs(unlist(serial[[k]]$probabilities) -
              unlist(concurrent[[k]]$probabilities)))
  }, numeric(1))
  if (any(differences > 1e-14)) {
    stop("RF serial/parallel smoke check failed.", call. = FALSE)
  }
  data.frame(fold = seq_along(tasks), max_absolute_difference = differences,
              passed = TRUE)
}
})
# END EMBEDDED SOURCE: R/p4_rf_functions.R

# BEGIN EMBEDDED SOURCE: R/p4_comparison_helpers.R
.submission_modules[["R/p4_comparison_helpers.R"]] <- quote({
# P4-only comparison contracts and paired descriptive diagnostics.
# Source R/p4_helpers.R first. No function in this file fits or tunes a model.

p4_model_prefix <- function(model) {
  switch(model, "Logistic Regression" = "logistic", "Random Forest" = "random_forest",
         "XGBoost" = "xgboost", stop("Unknown model: ", model))
}

p4_model_aliases <- function(model) {
  switch(model, "Logistic Regression" = c("logisticregression", "logistic", "lr"),
         "Random Forest" = c("randomforest", "rf"), "XGBoost" = c("xgboost", "xgb"))
}
p4_context_key <- function(x) gsub("[^a-z0-9]", "", tolower(trimws(as.character(x))))

# Runtime contexts distinguish CV fold times from final-model fitting and
# locked-test prediction time.
p4_read_model_times <- function(model, metadata = NULL, tables_dir = "outputs/tables",
                                native_params = NULL) {
  keys <- c("Training_time_s", "Prediction_time_s", "Tuning_time_s")
  answer <- setNames(rep(NA_real_, length(keys)), keys)
  prefix <- p4_model_prefix(model)
  files <- file.path(tables_dir, paste0(prefix, c("_metrics.csv", "_evaluation_metrics.csv")))
  files <- files[file.exists(files)]
  records <- data.frame(metric = character(), value = numeric(), context = character(),
                        source = character(), kind = character(), stringsAsFactors = FALSE)
  notes <- character()
  parse_seconds <- function(x) suppressWarnings(as.numeric(sub(
    "[[:space:]]*(seconds?|secs?|s)[[:space:]]*$", "", tolower(trimws(as.character(x))))))
  append_values <- function(key, values, context, source, kind = "csv") {
    values <- parse_seconds(values)
    valid <- is.finite(values) & values >= 0
    if (any(valid)) records <<- rbind(records, data.frame(
      metric = key, value = values[valid], context = context, source = source, kind = kind,
      stringsAsFactors = FALSE))
  }
  for (path in files) {
    tab <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
    if (all(c("metric", "value") %in% names(tab))) {
      if ("model" %in% names(tab)) tab <- tab[p4_context_key(tab$model) %in% p4_model_aliases(model), , drop = FALSE]
      for (fold_column in intersect(c("fold", "fold_id"), names(tab))) {
        fold <- trimws(as.character(tab[[fold_column]]))
        actual_fold <- !is.na(fold) & nzchar(fold) & !tolower(fold) %in% c("0", "na", "none", "final", "test")
        tab <- tab[!actual_fold, , drop = FALSE]
      }
      context_column <- intersect(c("dataset", "context", "phase", "scope"), names(tab))
      if (!length(context_column)) {
        notes <- c(notes, paste(path, "has no runtime context; ignored."))
        next
      }
      contexts <- p4_context_key(tab[[context_column[1L]]])
      for (key in keys) {
        allowed <- if (key == "Training_time_s")
          c("test", "final", "finalfit", "finaltraining", "fulltraining", "finalmodel", "lockedtest", "heldouttest") else
          if (key == "Prediction_time_s") c("test", "final", "finalprediction", "lockedtest", "heldouttest") else
            c("test", "final", "tuning", "cvtuning", "totaltuning", "hyperparametertuning")
        ix <- which(tab$metric == key & contexts %in% allowed)
        if (length(ix)) append_values(key, tab$value[ix], "final/test", path)
      }
    } else if ("Metric" %in% names(tab)) {
      # Logistic metrics identify locked-test results with named columns.
      test_columns <- grep("^Test([[:space:]]|$)", names(tab))
      labels <- c(Training_time_s = "Training Time (s)",
                  Prediction_time_s = "Prediction Time (s)", Tuning_time_s = "Tuning Time (s)")
      for (key in keys) for (j in test_columns) {
        ix <- which(tab$Metric == labels[[key]])
        if (length(ix)) append_values(key, tab[ix, j], names(tab)[j], path, "wide")
      }
    } else if (model == "XGBoost" && "Set" %in% names(tab)) {
      # XGBoost's row-wise export repeats final-fit time on OOF and test
      # rows. Only the exact tuned locked-test row is comparison evidence;
      # neither OOF_train nor the alternative Test_0.50 threshold is accepted.
      if ("model" %in% names(tab)) {
        tab <- tab[p4_context_key(tab$model) %in% p4_model_aliases(model), , drop = FALSE]
      }
      for (fold_column in intersect(c("fold", "fold_id"), names(tab))) {
        fold <- trimws(as.character(tab[[fold_column]]))
        actual_fold <- !is.na(fold) & nzchar(fold) &
          !tolower(fold) %in% c("0", "na", "none", "final", "test")
        tab <- tab[!actual_fold, , drop = FALSE]
      }
      tuned <- which(!is.na(tab$Set) & tab$Set == "Test_tuned")
      native_columns <- c(Training_time_s = "Train_Time_Sec",
                           Prediction_time_s = "Pred_Time_Sec")
      for (key in names(native_columns)) {
        column <- native_columns[[key]]
        if (length(tuned) && column %in% names(tab)) {
          # Zero is a valid exported timer-resolution result, not a missing
          # value. No tuning duration is inferred from these final-fit times.
          append_values(key, tab[tuned, column], "Test_tuned", path)
        }
      }
    }
  }
  # Optional owner metadata has explicitly separated timing fields in seconds.
  if (is.list(metadata) && !is.null(metadata$timing_seconds)) {
    for (key in intersect(keys, names(metadata$timing_seconds))) {
      append_values(key, metadata$timing_seconds[[key]], "owner final/test metadata", "run_metadata", "metadata")
    }
  }
  sources <- lapply(keys, function(key) {
    rows <- records[records$metric == key, , drop = FALSE]
    distinct <- unique(rows$value)
    native_value <- NULL
    if (model == "Logistic Regression" && is.list(native_params)) {
      native_keys <- c(Training_time_s = "training_time_s", Prediction_time_s = "prediction_time_s",
                       Tuning_time_s = "tuning_time_s")
      value <- native_params[[native_keys[[key]]]]
      valid <- is.numeric(value) && length(value) == 1L && is.finite(value) && value >= 0
      wide_values <- rows$value[rows$kind == "wide"]
      corroborated <- valid && length(wide_values) > 0L &&
        all(round(wide_values, 4L) == round(value, 4L))
      # Only the caller that verified the native model/OOF/table artifacts may
      # set this attribute. This function never reads a global params file.
      validated <- isTRUE(attr(native_params, "p4_native_verified"))
      if (valid && (corroborated || validated || length(wide_values) > 0L)) {
        native_value <- value
        native_source <- attr(native_params, "p4_native_path")
        if (!is.character(native_source) || length(native_source) != 1L || !nzchar(native_source))
          native_source <- "native Logistic parameters"
        rows <- rbind(rows, data.frame(metric = key, value = value,
          context = "native final/test timing", source = native_source, kind = "native",
          stringsAsFactors = FALSE))
        # The wide P3 table prints four decimal places. Require exact agreement
        # at that published precision before retaining the native precise value.
        agreement <- all(round(rows$value[rows$kind == "wide"], 4L) == round(value, 4L)) &&
          all(rows$value[rows$kind %in% c("csv", "metadata")] == value)
        distinct <- if (agreement) value else unique(rows$value)
      } else if (valid) notes <<- c(notes, paste(model, key,
        "native timing was not corroborated by a locked-test table or verified native artifacts; ignored."))
    }
    if (length(distinct) == 1L) answer[key] <<- if (is.null(native_value)) distinct else native_value
    status <- if (!length(distinct)) "not exported in an accepted context" else
      if (length(distinct) == 1L) "available" else "conflicting accepted runtime values; left missing"
    if (length(distinct) > 1L) notes <<- c(notes, paste(model, key, status))
    data.frame(model = model, metric = key, value = unname(answer[key]), status = status,
               source = paste(unique(rows$source), collapse = "; "), stringsAsFactors = FALSE)
  })
  list(values = answer, sources = do.call(rbind, sources), input_files = files,
       notes = unique(notes))
}

p4_exact_mcnemar <- function(predictions) {
  out <- data.frame(model_a = character(), model_b = character(),
                    a_correct_b_wrong = integer(), a_wrong_b_correct = integer(),
                    discordant = integer(), p_value = numeric(), p_adjusted = numeric())
  if (length(predictions) < 2L) return(out)
  out <- do.call(rbind, lapply(utils::combn(names(predictions), 2L, simplify = FALSE), function(pair) {
    a <- predictions[[pair[1L]]]; b <- predictions[[pair[2L]]]
    stopifnot(identical(a$sample_id, b$sample_id), identical(a$actual, b$actual))
    ca <- a$actual == a$predicted; cb <- b$actual == b$predicted
    n10 <- sum(ca & !cb); n01 <- sum(!ca & cb); n <- n10 + n01
    data.frame(model_a = pair[1L], model_b = pair[2L],
               a_correct_b_wrong = n10, a_wrong_b_correct = n01, discordant = n,
               p_value = if (!n) 1 else stats::binom.test(n10, n, p = 0.5)$p.value)
  }))
  out$p_adjusted <- stats::p.adjust(out$p_value, method = "holm")
  out
}

p4_pairwise_diagnostics <- function(predictions, test) {
  errors <- data.frame(model_a = character(), model_b = character(), actual = integer(),
                       class = character(), n = integer(), both_correct = integer(),
                       both_wrong = integer(), a_only_correct = integer(), b_only_correct = integer())
  disagreements <- data.frame(sample_id = integer(), actual = integer(), model_a = character(),
    model_b = character(), predicted_a = integer(), predicted_b = integer(),
    probability_ad_a = numeric(), probability_ad_b = numeric(), a_correct = logical(),
    b_correct = logical(), pattern = character(), geom_missing = integer(),
    local_missing = integer(), local = numeric(), height = numeric(), width = numeric(), aratio = numeric())
  costs <- data.frame(model_a = character(), model_b = character(), n_test = integer(),
    FP_a = integer(), FN_a = integer(), FP_b = integer(), FN_b = integer(),
    delta_FP_B_minus_A = integer(), delta_FN_B_minus_A = integer(),
    FN_to_FP_cost_crossover = numeric(), lower_cost_below_crossover = character(),
    lower_cost_above_crossover = character(), point_estimate_relation = character(), note = character())
  if (length(predictions) < 2L) return(list(paired_errors = errors, disagreements = disagreements, cost_tradeoffs = costs))
  erows <- drows <- crows <- list()
  for (pair in utils::combn(names(predictions), 2L, simplify = FALSE)) {
    a <- predictions[[pair[1L]]]; b <- predictions[[pair[2L]]]
    stopifnot(identical(a$sample_id, b$sample_id), identical(a$actual, b$actual),
              identical(a$sample_id, test$obs_id))
    ca <- a$actual == a$predicted; cb <- b$actual == b$predicted
    for (label in c(1L, 0L)) {
      ix <- a$actual == label
      erows[[length(erows) + 1L]] <- data.frame(
        model_a = pair[1L], model_b = pair[2L], actual = label,
        class = if (label == 1L) "ad" else "nonad", n = sum(ix),
        both_correct = sum(ix & ca & cb), both_wrong = sum(ix & !ca & !cb),
        a_only_correct = sum(ix & ca & !cb), b_only_correct = sum(ix & !ca & cb))
    }
    ix <- which(a$predicted != b$predicted)
    if (length(ix)) {
      get_test <- function(v) if (v %in% names(test)) test[[v]][ix] else rep(NA_real_, length(ix))
      geometric <- if (all(NUMERIC_VARS %in% names(test)))
        as.integer(rowSums(is.na(test[ix, NUMERIC_VARS, drop = FALSE])) > 0L) else rep(NA_integer_, length(ix))
      drows[[length(drows) + 1L]] <- data.frame(
        sample_id = a$sample_id[ix], actual = a$actual[ix], model_a = pair[1L], model_b = pair[2L],
        predicted_a = a$predicted[ix], predicted_b = b$predicted[ix],
        probability_ad_a = a$probability_ad[ix], probability_ad_b = b$probability_ad[ix],
        a_correct = ca[ix], b_correct = cb[ix],
        pattern = ifelse(ca[ix], "A correct; B wrong", "A wrong; B correct"),
        geom_missing = geometric, local_missing = as.integer(is.na(get_test("local"))),
        local = get_test("local"), height = get_test("height"), width = get_test("width"), aratio = get_test("aratio"))
    }
    ma <- p4_metrics(a$actual, a$probability_ad, a$predicted)
    mb <- p4_metrics(b$actual, b$probability_ad, b$predicted)
    dfp <- unname(mb["FP"] - ma["FP"]); dfn <- unname(mb["FN"] - ma["FN"])
    crossover <- if (dfp * dfn < 0) -dfp / dfn else NA_real_
    relation <- if (dfp == 0 && dfn == 0) "same aggregate FP/FN counts" else
      if (dfp <= 0 && dfn <= 0) "B has no more FP or FN at these fixed labels" else
        if (dfp >= 0 && dfn >= 0) "A has no more FP or FN at these fixed labels" else "opposing FP/FN trade-off"
    crows[[length(crows) + 1L]] <- data.frame(
      model_a = pair[1L], model_b = pair[2L], n_test = nrow(a),
      FP_a = unname(ma["FP"]), FN_a = unname(ma["FN"]), FP_b = unname(mb["FP"]), FN_b = unname(mb["FN"]),
      delta_FP_B_minus_A = dfp, delta_FN_B_minus_A = dfn, FN_to_FP_cost_crossover = crossover,
      lower_cost_below_crossover = if (is.finite(crossover)) if (dfn < 0) pair[1L] else pair[2L] else NA_character_,
      lower_cost_above_crossover = if (is.finite(crossover)) if (dfn < 0) pair[2L] else pair[1L] else NA_character_,
      point_estimate_relation = relation,
      note = "Descriptive fixed-label test cost: loss(B)-loss(A) = delta_FP*c_FP + delta_FN*c_FN. Costs are not estimated; no threshold, seed or model is selected here.")
  }
  list(paired_errors = do.call(rbind, erows),
       disagreements = if (length(drows)) do.call(rbind, drows) else disagreements,
       cost_tradeoffs = do.call(rbind, crows))
}

# Run metadata is optional for numerical comparison, but protocol statements
# cannot be inferred from a four-column prediction file.
p4_read_run_metadata <- function(model, models_dir = "outputs/models") {
  path <- file.path(models_dir, paste0(p4_model_prefix(model), "_run_metadata.rds"))
  if (!file.exists(path)) return(list(data = NULL, path = path, exists = FALSE, issue = "run metadata not exported"))
  value <- tryCatch(readRDS(path), error = function(e) e)
  if (!is.list(value) || inherits(value, "error")) return(list(
    data = NULL, path = path, exists = TRUE, issue = "run metadata is not a readable RDS list"))
  list(data = value, path = path, exists = TRUE, issue = "")
}

p4_provider_protocol <- function(metadata, shared, prediction = NULL) {
  reasons <- character()
  scalar_flag <- function(name, value) {
    if (!identical(metadata[[name]], value)) reasons <<- c(reasons, paste(name, "not confirmed"))
  }
  if (!is.list(metadata)) return(list(verified = FALSE, reasons = "owner protocol metadata missing"))
  scalar_flag("training_only_preprocessing", TRUE)
  scalar_flag("shared_cv_folds_used", TRUE)
  scalar_flag("threshold_selected_on_training_only", TRUE)
  scalar_flag("test_used_for_tuning", FALSE)
  threshold <- metadata$threshold
  if (!is.numeric(threshold) || length(threshold) != 1L || !is.finite(threshold) || threshold < 0 || threshold > 1) {
    reasons <- c(reasons, "valid saved threshold missing")
  }
  seed <- metadata$seed
  if (!is.numeric(seed) || length(seed) != 1L || !is.finite(seed) || seed != round(seed)) reasons <- c(reasons, "training seed missing")
  alignment_paths <- c("data/processed/train_raw.rds", "data/processed/test_raw.rds", "data/processed/cv_folds.rds")
  hashes <- metadata$input_md5
  hashes_match <- is.character(hashes) && !is.null(names(hashes)) &&
    all(alignment_paths %in% names(hashes)) &&
    identical(tolower(unname(hashes[alignment_paths])), tolower(unname(tools::md5sum(alignment_paths))))
  ids_match <- !is.null(metadata$train_ids) && !is.null(metadata$test_ids) && !is.null(metadata$cv_fold_id) &&
    identical(as.integer(metadata$train_ids), as.integer(shared$train$obs_id)) &&
    identical(as.integer(metadata$test_ids), as.integer(shared$test$obs_id)) &&
    identical(as.integer(metadata$cv_fold_id), as.integer(shared$folds$fold_id))
  if (!hashes_match) reasons <- c(reasons, "training/test/fold file hashes not matched")
  if (any(c("train_ids", "test_ids", "cv_fold_id") %in% names(metadata)) && !ids_match) {
    reasons <- c(reasons, "optional training/test/fold ID metadata contradicts current inputs")
  }
  if (!is.null(prediction) && is.numeric(threshold) && length(threshold) == 1L && is.finite(threshold)) {
    rule <- metadata$threshold_rule
    if (!is.character(rule) || length(rule) != 1L || !rule %in% c(">=", ">")) reasons <- c(reasons, "threshold_rule missing (>= or >)") else {
      labels <- if (rule == ">=") as.integer(prediction$probability_ad >= threshold) else as.integer(prediction$probability_ad > threshold)
      if (!identical(labels, prediction$predicted)) reasons <- c(reasons, "saved labels disagree with threshold and exported probabilities")
    }
  }
  list(verified = !length(reasons), reasons = unique(reasons))
}

p4_verify_rf_artifacts <- function(shared, prediction, models_dir = "outputs/models", predictions_dir = "outputs/predictions") {
  paths <- c(model = file.path(models_dir, "random_forest_model.rds"),
             params = file.path(models_dir, "random_forest_params.rds"),
             oof = file.path(predictions_dir, "random_forest_oof_predictions.csv"))
  present <- file.exists(paths)
  result <- list(verified = FALSE, reasons = paste(names(paths)[!present], "artifact missing"), input_files = unname(paths[present]))
  if (!all(present) || is.null(prediction)) return(result)
  checked <- tryCatch({
    params <- readRDS(paths["params"]); bundle <- readRDS(paths["model"])
    required <- c("ntree", "mtry", "nodesize", "threshold", "seed", "opts")
    stopifnot(is.list(params), is.list(bundle), all(c("model", "prep", "params") %in% names(bundle)),
              all(required %in% names(params)), all(required %in% names(bundle$params)),
              identical(params[required], bundle$params[required]),
              identical(params$cv_folds, shared$folds$k),
              identical(params$cv_seed_rule, "seed + fold - 1"),
              inherits(bundle$prep, "ad_prep"), bundle$prep$n_train == nrow(shared$train))
    oof <- p4_read_predictions(paths["oof"], shared$train)
    raw <- read.csv(paths["oof"], stringsAsFactors = FALSE)
    raw_id <- if ("sample_id" %in% names(raw)) "sample_id" else "obs_id"
    raw <- raw[match(shared$train$obs_id, raw[[raw_id]]), , drop = FALSE]
    stopifnot("fold" %in% names(raw), identical(as.integer(raw$fold), as.integer(shared$folds$fold_id)),
              identical(oof$predicted, as.integer(oof$probability_ad >= params$threshold)),
              abs(p4_choose_threshold(oof$actual, oof$probability_ad)$threshold - params$threshold) < 1e-12)
    if (!requireNamespace("randomForest", quietly = TRUE)) stop("randomForest package needed to verify saved RF predictions")
    transformed <- prep_apply(bundle$prep, shared$test, target = "factor")
    reproduced <- as.numeric(predict(bundle$model, transformed[, bundle$model$p4_predictors, drop = FALSE], type = "prob")[, "ad"])
    stopifnot(isTRUE(all.equal(reproduced, prediction$probability_ad, tolerance = 1e-12)),
              identical(prediction$predicted, as.integer(reproduced >= params$threshold)))
    TRUE
  }, error = function(e) conditionMessage(e))
  result$verified <- isTRUE(checked)
  result$reasons <- if (isTRUE(checked)) character() else paste("RF artifact verification failed:", checked)
  result
}

p4_comparison_readiness <- function(model_names, predictions, shared, timings, metadata,
                                    models_dir = "outputs/models", predictions_dir = "outputs/predictions",
                                    tables_dir = "outputs/tables", native_checks = NULL) {
  files <- character(); rows <- vector("list", length(model_names))
  for (i in seq_along(model_names)) {
    model <- model_names[i]; prefix <- p4_model_prefix(model)
    model_path <- file.path(models_dir, paste0(prefix, "_model.rds"))
    params_path <- file.path(models_dir, paste0(prefix, "_params.rds"))
    pred <- predictions[[model]]; meta <- metadata[[model]]
    reasons <- character()
    native <- NULL
    if (is.null(pred)) reasons <- c(reasons, "test predictions pending")
    model_present <- file.exists(model_path); params_present <- file.exists(params_path)
    files <- c(files, c(model_path, params_path)[c(model_present, params_present)])
    if (!model_present) reasons <- c(reasons, "saved model not exported")
    if (!params_present) reasons <- c(reasons, "saved parameters not exported")
    if (meta$exists) files <- c(files, meta$path)
    if (model == "Random Forest") {
      local <- p4_verify_rf_artifacts(shared, pred, models_dir, predictions_dir)
      files <- c(files, local$input_files)
      protocol <- local$verified
      evidence <- if (protocol) "local RF implementation and saved artifacts verified" else "local RF artifacts not verified"
      reasons <- c(reasons, local$reasons)
      reproducible <- local$verified
    } else if ((model == "XGBoost" && exists("p4_verify_xgboost_artifacts", mode = "function")) ||
               (model == "Logistic Regression" && exists("p4_verify_logistic_artifacts", mode = "function"))) {
      native <- native_checks[[model]]
      if (is.null(native)) {
        verify <- if (model == "XGBoost") p4_verify_xgboost_artifacts else p4_verify_logistic_artifacts
        native <- verify(shared, pred, models_dir, predictions_dir, tables_dir)
      }
      files <- c(files, native$input_files)
      protocol_check <- p4_provider_protocol(meta$data, shared, pred)
      protocol <- protocol_check$verified
      reproducible <- isTRUE(native$verified)
      reasons <- c(reasons, native$reasons)
      if (!protocol) reasons <- c(reasons, protocol_check$reasons, if (reproducible)
        "native artifacts verified; owner protocol attestation and historical input hashes remain incomplete")
      evidence <- if (reproducible)
        if (model == "Logistic Regression")
          "native weighted ridge Logistic (alpha = 0), reconstructed training-only preprocessing, model and OOF threshold verified; historical folds not replayed" else
            "native XGBoost preprocessing/model/OOF threshold verified; historical fold hashes unavailable" else
          paste("native", model, "artifact verification incomplete")
    } else {
      protocol_check <- p4_provider_protocol(meta$data, shared, pred)
      protocol <- protocol_check$verified
      reasons <- c(reasons, protocol_check$reasons)
      if (nzchar(meta$issue)) reasons <- c(reasons, meta$issue)
      evidence <- if (protocol) "owner metadata attests protocol; training not rerun by P4" else "protocol unknown or metadata incomplete"
      reproducible <- model_present && params_present && protocol
    }
    software <- model == "Random Forest" ||
      (isTRUE(native$verified) && length(native$metadata$software_versions) > 0L) ||
      (model == "XGBoost" && isTRUE(native$verified) && is.character(native$params$xgboost_version) && length(native$params$xgboost_version) == 1L) ||
      (is.list(meta$data) && length(meta$data$software_versions) > 0L)
    # LR and RF timer boundaries have been audited in their source scripts.
    boundaries <- model %in% c("Logistic Regression", "Random Forest") || (is.list(meta$data) && is.character(meta$data$timing_boundaries) &&
                                                any(nzchar(meta$data$timing_boundaries))) || isTRUE(native$verified)
    time_values <- timings[[model]]$values
    runtime <- all(is.finite(time_values[c("Training_time_s", "Prediction_time_s")]))
    if (!software) reasons <- c(reasons, "software versions not exported")
    if (!boundaries) reasons <- c(reasons, "timing boundaries not documented")
    if (!runtime) reasons <- c(reasons, "final fit or test-prediction time unavailable")
    ready <- !is.null(pred) && reproducible && protocol && software && boundaries && runtime
    rows[[i]] <- data.frame(
      model = model, predictions_available = !is.null(pred), ids_labels_verified = !is.null(pred),
      model_available = model_present, params_available = params_present,
      run_metadata_available = meta$exists, protocol_ready = protocol,
      native_artifacts_verified = if (model == "Random Forest") reproducible else isTRUE(native$verified),
      protocol_verified_or_attested = protocol,
      metadata_status = if (model == "Random Forest" && protocol) "local RF artifacts verified" else
        if (protocol) "owner protocol attested" else if (isTRUE(native$verified)) paste("native", model, "artifacts verified; owner attestation incomplete") else
          if (meta$exists) "incomplete or invalid owner metadata" else "owner metadata missing",
      reproducibility_ready = reproducible, runtime_complete = runtime,
      final_selection_ready = ready, evidence = evidence,
      missing_details = paste(unique(reasons), collapse = "; "),
      pending_reasons = paste(unique(reasons), collapse = "; "), stringsAsFactors = FALSE)
  }
  list(table = do.call(rbind, rows), input_files = unique(files))
}
})
# END EMBEDDED SOURCE: R/p4_comparison_helpers.R

# BEGIN EMBEDDED SOURCE: R/p4_logistic_artifacts.R
.submission_modules[["R/p4_logistic_artifacts.R"]] <- quote({
# Native P3 consistency checks. Source R/p4_helpers.R first.
# No fitting, package installation, or historical CV replay occurs here.
p4_verify_logistic_artifacts <- function(shared, prediction,
                                         models_dir = "outputs/models",
                                         predictions_dir = "outputs/predictions",
                                         tables_dir = "outputs/tables") {
  result <- list(verified = FALSE, reasons = character(), input_files = character(),
    params = NULL, metadata = list(native_metadata_available = FALSE,
      historical_input_hashes_available = FALSE, historical_folds_replayed = FALSE,
      fitted_preprocessing_saved = FALSE, preprocessing_reconstructed = FALSE,
      software_versions_available = FALSE, software_versions = character(),
      scope = "saved ridge model, reconstructed current training-only preprocessing, exported test/OOF and timing consistency; no historical CV replay"))
  paths <- c(params = file.path(models_dir, "logistic_params.rds"),
             model = file.path(models_dir, "logistic_model.rds"),
             oof = file.path(predictions_dir, "logistic_oof_predictions.csv"))
  check <- function(ok, message) if (!isTRUE(ok)) stop(message, call. = FALSE)
  same_numeric <- function(actual, expected, label, tolerance = 1e-12) {
    check(is.numeric(actual) && is.numeric(expected) && length(actual) == length(expected) &&
            all(is.finite(actual)) && all(is.finite(expected)) &&
            all(abs(as.numeric(actual) - as.numeric(expected)) <= tolerance),
          paste("Native Logistic", label, "does not match."))
  }
  read_native <- function(path, rds = FALSE) {
    check(file.exists(path), paste("Native Logistic artifact missing:", path))
    result$input_files <<- unique(c(result$input_files, unname(path)))
    if (rds) readRDS(path) else read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  }
  align_prediction <- function(data, reference, label, fold = FALSE) {
    columns <- c("actual", "predicted", "probability_ad", if (fold) "fold")
    check(is.data.frame(data) && !anyDuplicated(names(data)) && all(columns %in% names(data)),
          paste("Native Logistic", label, "has missing or duplicate columns."))
    id <- if ("sample_id" %in% names(data)) "sample_id" else "obs_id"
    check(id %in% names(data) && !anyNA(data[[id]]) && !anyDuplicated(data[[id]]) &&
            nrow(data) == nrow(reference) && setequal(data[[id]], reference$obs_id),
          paste("Native Logistic", label, "IDs do not match the shared observations."))
    if (all(c("sample_id", "obs_id") %in% names(data))) {
      check(identical(as.character(data$sample_id), as.character(data$obs_id)),
            paste("Native Logistic", label, "ID aliases disagree."))
    }
    data <- data[match(reference$obs_id, data[[id]]), , drop = FALSE]
    p4_metrics(data$actual, data$probability_ad, data$predicted)
    check(identical(as.integer(data$actual), as.integer(reference$y)),
          paste("Native Logistic", label, "true labels do not match."))
    data
  }
  checked <- tryCatch({
    params <- read_native(paths[["params"]], rds = TRUE)
    result$params <- params
    required <- c("best_lambda", "best_threshold", "weights", "opts",
                  "training_time_s", "prediction_time_s", "tuning_time_s")
    check(is.list(params) && all(required %in% names(params)),
          "Native Logistic parameter metadata is incomplete.")
    result$metadata$native_metadata_available <- TRUE
    check(is.numeric(params$best_lambda) && length(params$best_lambda) == 1L &&
            is.finite(params$best_lambda) && params$best_lambda > 0,
          "Native Logistic lambda must be a positive scalar.")
    check(is.numeric(params$best_threshold) && length(params$best_threshold) == 1L &&
            is.finite(params$best_threshold) && params$best_threshold >= 0 && params$best_threshold <= 1,
          "Native Logistic threshold must be a probability scalar.")
    for (key in c("training_time_s", "prediction_time_s", "tuning_time_s")) {
      check(is.numeric(params[[key]]) && length(params[[key]]) == 1L &&
              is.finite(params[[key]]) && params[[key]] >= 0,
            paste("Native Logistic", key, "must be a nonnegative duration."))
    }
    result$metadata$alpha <- 0
    result$metadata$lambda <- params$best_lambda
    result$metadata$threshold <- params$best_threshold
    result$metadata$threshold_rule <- ">="
    result$metadata$timing_seconds <- c(Training_time_s = params$training_time_s,
      Prediction_time_s = params$prediction_time_s, Tuning_time_s = params$tuning_time_s)
    # Boundaries audited from P3's R/07_logistic_regression.R, not a historical timer log.
    result$metadata$timing_boundaries <- c(
      Training_time_s = "final glmnet fit only; preprocessing excluded",
      Prediction_time_s = "locked-test model response only; preprocessing excluded",
      Tuning_time_s = "5-fold preprocessing, weighted ridge lambda-grid fits and OOF prediction; threshold scan excluded")
    check(is.list(params$opts) && isTRUE(all.equal(params$opts, prep_options("logistic"), tolerance = 1e-12)),
          "Native Logistic preprocessing options differ from the current logistic preset.")
    same_numeric(params$weights, case_weights(shared$train$y), "final case weights")
    check(requireNamespace("Matrix", quietly = TRUE),
          "Matrix is required to inspect native Logistic sparse coefficients.")
    model <- read_native(paths[["model"]], rds = TRUE)
    check(inherits(model, "lognet") && inherits(model, "glmnet"),
          "Native Logistic saved model is not a binomial glmnet model.")
    check(is.call(model$call) && is.numeric(model$call$alpha) &&
            length(model$call$alpha) == 1L && model$call$alpha == 0 &&
            identical(model$call$family, "binomial"),
          "Native Logistic saved-model configuration must be binomial ridge alpha=0.")
    same_numeric(model$lambda, params$best_lambda, "saved-model lambda")
    same_numeric(model$nobs, nrow(shared$train), "saved-model training count")
    check(identical(model$offset, FALSE) && identical(model$classnames, c("0", "1")) &&
            is.numeric(model$jerr) && length(model$jerr) == 1L && model$jerr == 0,
          "Native Logistic saved-model offset, classes or fitting status is invalid.")
    # P3 saved options and coefficients, but no fitted preprocessing object.
    # Reconstruct solely from the CURRENT shared training observations.
    reconstructed <- prep_fit(shared$train, params$opts)
    matrix <- prep_matrix(reconstructed, shared$test)$x
    check(inherits(model$beta, "dgCMatrix") && ncol(model$beta) == 1L &&
            nrow(model$beta) == ncol(matrix) &&
            identical(rownames(model$beta), colnames(matrix)),
          "Native Logistic saved-model features differ from reconstructed preprocessing.")
    same_numeric(model$dim, c(ncol(matrix), 1), "saved-model dimensions")
    check(is.numeric(model$a0) && length(model$a0) == 1L && is.finite(model$a0) &&
            all(is.finite(as.numeric(model$beta))), "Native Logistic coefficients are invalid.")
    aligned <- align_prediction(prediction, shared$test, "test predictions")
    reproduced <- stats::plogis(as.numeric(model$a0 + matrix %*% model$beta))
    same_numeric(aligned$probability_ad, round(reproduced, 6), "six-decimal saved-model test probabilities")
    check(identical(as.integer(aligned$predicted), as.integer(reproduced >= params$best_threshold)),
          "Native Logistic test labels disagree with the full-precision model threshold.")
    oof <- align_prediction(read_native(paths[["oof"]]), shared$train, "OOF predictions", fold = TRUE)
    check(is.numeric(oof$fold) && identical(as.integer(oof$fold), as.integer(shared$folds$fold_id)) &&
            identical(as.numeric(oof$fold), as.numeric(shared$folds$fold_id)) &&
            length(shared$folds$indexOut) == shared$folds$k &&
            all(vapply(seq_len(shared$folds$k), function(k)
              identical(which(oof$fold == k), as.integer(shared$folds$indexOut[[k]])), logical(1))),
          "Native Logistic OOF folds do not match the shared locked folds.")
    check(identical(as.integer(oof$predicted), as.integer(oof$probability_ad >= params$best_threshold)),
          "Native Logistic OOF labels disagree with the saved threshold.")
    thresholds <- seq(0.10, 0.90, by = 0.01)
    f1 <- vapply(thresholds, function(th)
      unname(p4_metrics(oof$actual, oof$probability_ad, threshold = th)["F1"]), numeric(1))
    same_numeric(params$best_threshold, thresholds[which.max(f1)], "first-max-F1 OOF threshold")

    # Native exact Shape A times and four-decimal wide Test times describe the
    # same run; compare at each export's precision rather than exact equality.
    for (name in c("logistic_metrics.csv", "logistic_evaluation_metrics.csv")) {
      path <- file.path(tables_dir, name)
      if (!file.exists(path)) next
      tab <- read_native(path)
      for (key in names(result$metadata$timing_seconds)) {
        expected <- result$metadata$timing_seconds[[key]]
        if (name == "logistic_metrics.csv") {
          check(all(c("metric", "value") %in% names(tab)) && sum(tab$metric == key) == 1L,
                paste("Native Logistic exact runtime table is missing", key))
          same_numeric(tab$value[tab$metric == key], expected, paste("exact runtime", key))
        } else {
          label <- c(Training_time_s = "Training Time (s)", Prediction_time_s = "Prediction Time (s)",
                     Tuning_time_s = "Tuning Time (s)")[[key]]
          columns <- grep("^Test([[:space:]]|$)", names(tab))
          check("Metric" %in% names(tab) && sum(tab$Metric == label) == 1L && length(columns) > 0L,
                paste("Native Logistic wide runtime table is missing", key))
          values <- suppressWarnings(as.numeric(sub("[[:space:]]*s$", "",
            as.character(unlist(tab[tab$Metric == label, columns], use.names = FALSE)))))
          same_numeric(values, rep(round(expected, 4), length(columns)), paste("rounded runtime", key))
        }
      }
    }
    result$metadata$preprocessing_reconstructed <- TRUE
    result$metadata$n_train <- nrow(shared$train)
    result$metadata$n_features <- ncol(matrix)
    result$metadata$historical_limitations <- paste(
      "No historical input/fold hashes, fitted preprocessing object, software versions or full lambda-selection grid were exported.",
      "Current training-only preprocessing was reconstructed; historical CV fits and lambda optimality were not replayed.")
    TRUE
  }, error = function(error) conditionMessage(error))
  result$verified <- isTRUE(checked)
  result$reasons <- if (isTRUE(checked)) character() else paste("Native Logistic artifact check failed:", checked)
  result
}
})
# END EMBEDDED SOURCE: R/p4_logistic_artifacts.R

# BEGIN EMBEDDED SOURCE: R/p4_xgboost_artifacts.R
.submission_modules[["R/p4_xgboost_artifacts.R"]] <- quote({
# Native P5 artifact consistency checks. Source R/p4_helpers.R first.
# This file never trains XGBoost or reruns the owner's historical CV folds.

p4_verify_xgboost_artifacts <- function(shared, prediction,
                                        models_dir = "outputs/models",
                                        predictions_dir = "outputs/predictions",
                                        tables_dir = "outputs/tables") {
  result <- list(
    verified = FALSE, reasons = character(), input_files = character(), params = NULL,
    metadata = list(native_metadata_available = FALSE,
      historical_input_hashes_available = FALSE, historical_folds_replayed = FALSE,
      scope = "saved model, current preprocessing, exported OOF and table consistency; historical CV folds were not rerun")
  )
  paths <- c(params = file.path(models_dir, "xgboost_params.rds"),
             model = file.path(models_dir, "xgboost_model.rds"),
             oof = file.path(predictions_dir, "xgboost_oof_predictions.csv"),
             cv = file.path(tables_dir, "xgboost_cv_grid.csv"),
             thresholds = file.path(tables_dir, "xgboost_threshold_tuning.csv"))
  read_native <- function(key, rds = FALSE) {
    path <- paths[[key]]
    if (!file.exists(path)) stop("Native XGBoost ", key, " artifact missing.", call. = FALSE)
    result$input_files <<- unique(c(result$input_files, path))
    if (rds) readRDS(path) else read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  }
  check <- function(condition, message) {
    if (!isTRUE(condition)) stop(message, call. = FALSE)
  }
  same_numeric <- function(actual, expected, label, tolerance = 1e-10) {
    check(is.numeric(actual) && is.numeric(expected) &&
            isTRUE(all.equal(as.numeric(actual), as.numeric(expected),
                             tolerance = tolerance, check.attributes = FALSE)),
          paste("Native XGBoost", label, "does not match."))
  }
  require_columns <- function(data, columns, label) {
    check(is.data.frame(data) && all(columns %in% names(data)),
          paste("Native XGBoost", label, "has missing columns."))
  }
  checked <- tryCatch({
    params <- read_native("params", rds = TRUE)
    result$params <- params
    result$metadata$native_metadata_available <- is.list(params)
    required <- c("best_config", "best_threshold", "prep", "cv_results", "params",
                   "train_time_sec", "pred_time_sec", "seed", "xgboost_version")
    check(is.list(params) && all(required %in% names(params)),
          "Native XGBoost parameter metadata is incomplete.")
    result$metadata$software_versions <- c(xgboost = as.character(params$xgboost_version))
    result$metadata$seed <- params$seed
    result$metadata$threshold <- params$best_threshold
    result$metadata$threshold_rule <- ">="
    result$metadata$timing_seconds <- c(Training_time_s = params$train_time_sec,
                                       Prediction_time_s = params$pred_time_sec)
    missing <- names(paths)[!file.exists(paths)]
    check(!length(missing), paste("Native XGBoost artifacts missing:", paste(missing, collapse = ", ")))
    check(requireNamespace("xgboost", quietly = TRUE),
          "xgboost 3.2.1.1 is required to safely read the native saved model.")
    installed <- as.character(utils::packageVersion("xgboost"))
    check(identical(installed, "3.2.1.1") &&
            identical(as.character(params$xgboost_version), "3.2.1.1"),
          "Exact installed and owner xgboost version 3.2.1.1 is required before reading the saved model.")
    check(is.numeric(params$seed) && length(params$seed) == 1L &&
            is.finite(params$seed) && params$seed == PROJECT_SEED,
          "Native XGBoost seed does not match the shared project seed.")
    check(is.list(params$params) && identical(params$params$objective, "binary:logistic") &&
            identical(params$params$eval_metric, "aucpr"),
          "Native XGBoost objective or evaluation configuration is inconsistent.")
    same_numeric(params$params$seed, params$seed, "engine seed")
    same_numeric(params$params$scale_pos_weight, scale_pos_weight(shared$train$y), "final class weight")
    for (key in c("train_time_sec", "pred_time_sec")) {
      value <- params[[key]]
      check(is.numeric(value) && length(value) == 1L && is.finite(value) && value >= 0,
            paste("Native XGBoost", key, "must be a nonnegative duration."))
    }

    expected_prep <- prep_fit(shared$train, prep_options("xgboost"))
    check(inherits(params$prep, "ad_prep") &&
            isTRUE(all.equal(params$prep, expected_prep, tolerance = 1e-12)),
          "Native XGBoost saved preprocessing differs from the current training-only xgboost preset.")
    config_columns <- c("eta", "max_depth", "nrounds", "min_child_weight",
                         "subsample", "colsample_bytree", "lambda", "alpha")
    require_columns(params$best_config, config_columns, "best_config")
    check(nrow(params$best_config) == 1L, "Native XGBoost best_config must have one row.")
    require_columns(params$cv_results, c("config", config_columns, "mean_fold_PR_AUC",
                                         "pooled_OOF_PR_AUC"), "saved CV results")
    check(nrow(params$cv_results) >= 1L &&
            all(is.finite(params$cv_results$mean_fold_PR_AUC)) &&
            !anyDuplicated(params$cv_results$config), "Native XGBoost saved CV grid is invalid.")
    best_index <- which.max(params$cv_results$mean_fold_PR_AUC)
    for (column in config_columns) {
      same_numeric(params$best_config[[column]], params$cv_results[[column]][best_index],
                    paste("selected", column))
      if (column != "nrounds") {
        same_numeric(params$params[[column]], params$best_config[[column]],
                      paste("engine", column))
      }
    }
    cv_export <- read_native("cv")
    check(identical(names(cv_export), names(params$cv_results)) &&
            nrow(cv_export) == nrow(params$cv_results),
          "Native XGBoost exported CV grid has a different shape from saved CV results.")
    for (column in names(params$cv_results)) {
      same_numeric(cv_export[[column]], params$cv_results[[column]], paste("CV column", column))
    }

    oof <- read_native("oof")
    require_columns(oof, c("actual", "probability_ad"), "OOF predictions")
    id_column <- if ("sample_id" %in% names(oof)) "sample_id" else "obs_id"
    check(id_column %in% names(oof), "Native XGBoost OOF observation IDs are missing.")
    ids <- oof[[id_column]]
    check(!anyNA(ids) && !anyDuplicated(ids) && nrow(oof) == nrow(shared$train) &&
            setequal(ids, shared$train$obs_id), "Native XGBoost OOF IDs do not match the shared training observations.")
    if (all(c("obs_id", "sample_id") %in% names(oof))) {
      check(identical(as.character(oof$obs_id), as.character(oof$sample_id)),
            "Native XGBoost OOF ID aliases disagree.")
    }
    oof <- oof[match(shared$train$obs_id, ids), , drop = FALSE]
    p4_validate_scores(oof$actual, oof$probability_ad)
    check(identical(as.integer(oof$actual), as.integer(shared$train$y)),
          "Native XGBoost OOF true labels do not match current training labels.")
    thresholds <- seq(0.10, 0.90, by = 0.01)
    scores <- t(vapply(thresholds, function(th) {
      p4_metrics(oof$actual, oof$probability_ad, threshold = th)
    }, numeric(11)))
    # P5 uses which.max(), so an exact tie selects the FIRST threshold, unlike
    # P4 RF's deliberately different nearest-0.5 tie convention.
    expected_threshold <- thresholds[which.max(scores[, "F1"])]
    same_numeric(params$best_threshold, expected_threshold, "first-max-F1 OOF threshold", tolerance = 1e-12)
    threshold_export <- read_native("thresholds")
    metric_map <- c(Accuracy = "Accuracy", Precision = "Precision", Recall = "Recall",
                    Specificity = "Specificity", F1_Score = "F1", ROC_AUC = "ROC_AUC",
                    TP = "TP", FP = "FP", FN = "FN", TN = "TN")
    require_columns(threshold_export, c("Threshold", names(metric_map), "PR_AUC", "Average_Precision"),
                    "threshold tuning table")
    same_numeric(threshold_export$Threshold, thresholds, "exported threshold grid")
    for (column in names(metric_map)) {
      same_numeric(threshold_export[[column]], scores[, metric_map[[column]]],
                    paste("threshold-table", column))
    }
    # Keep the owner's native PR/AP definitions separate from P4's common
    # tie-grouped PR-AUC. Cross-file checks do not relabel native AP as PR-AUC.
    same_numeric(threshold_export$PR_AUC,
                  rep(params$cv_results$pooled_OOF_PR_AUC[best_index], length(thresholds)),
                  "threshold-table native PR-AUC")
    ordered <- order(-oof$probability_ad, seq_len(nrow(oof)))
    yy <- oof$actual[ordered]
    native_ap <- mean((cumsum(yy == 1L) / seq_along(yy))[yy == 1L])
    same_numeric(threshold_export$Average_Precision, rep(native_ap, length(thresholds)),
                  "threshold-table native average precision")

    require_columns(prediction, c("sample_id", "actual", "predicted", "probability_ad"),
                    "locked-test predictions")
    check(!anyNA(prediction$sample_id) && !anyDuplicated(prediction$sample_id) &&
            nrow(prediction) == nrow(shared$test) &&
            setequal(prediction$sample_id, shared$test$obs_id),
          "Native XGBoost test prediction IDs do not match the locked test set.")
    aligned <- prediction[match(shared$test$obs_id, prediction$sample_id), , drop = FALSE]
    p4_metrics(aligned$actual, aligned$probability_ad, aligned$predicted)
    check(identical(as.integer(aligned$actual), as.integer(shared$test$y)),
          "Native XGBoost test true labels do not match the locked test labels.")
    check(identical(as.integer(aligned$predicted),
                     as.integer(aligned$probability_ad >= params$best_threshold)),
          "Native XGBoost test labels disagree with the saved threshold.")
    # Version compatibility has already been enforced before this readRDS.
    model <- read_native("model", rds = TRUE)
    check(inherits(model, "xgb.Booster"), "Native XGBoost saved model is not an xgb.Booster.")
    matrix <- prep_matrix(params$prep, shared$test)$x
    check(identical(variable.names(model), colnames(matrix)),
          "Native XGBoost model feature names/order differ from saved preprocessing.")
    same_numeric(xgboost::xgb.get.num.boosted.rounds(model), params$best_config$nrounds,
                  "saved-model boosting rounds")
    # Metadata/CV tables can agree with each other while describing a different
    # fitted booster. Check the serialized engine configuration as well as its
    # predictions. XGBoost stores numeric configuration values as float strings.
    model_config <- xgboost::xgb.config(model)
    learner <- model_config$learner
    check(identical(learner$objective$name, params$params$objective),
          "Native XGBoost saved-model objective differs from parameter metadata.")
    for (key in c("seed", "nthread")) {
      same_numeric(as.numeric(learner$generic_param[[key]]), params$params[[key]],
                    paste("saved-model generic parameter", key), tolerance = 1e-6)
    }
    tree_params <- learner$gradient_booster$tree_train_param
    for (key in c("eta", "max_depth", "min_child_weight", "subsample",
                  "colsample_bytree", "lambda", "alpha")) {
      same_numeric(as.numeric(tree_params[[key]]), params$params[[key]],
                    paste("saved-model tree parameter", key), tolerance = 1e-6)
    }
    same_numeric(as.numeric(learner$objective$reg_loss_param$scale_pos_weight),
                  params$params$scale_pos_weight, "saved-model class weight", tolerance = 1e-6)
    dtest <- xgboost::xgb.DMatrix(matrix, missing = NA)
    reproduced <- as.numeric(predict(model, dtest))
    same_numeric(reproduced, aligned$probability_ad, "saved-model test probabilities", tolerance = 1e-12)
    check(identical(as.integer(reproduced >= params$best_threshold), as.integer(aligned$predicted)),
          "Native XGBoost reproduced test labels disagree with exported labels.")
    result$metadata$best_config_index <- best_index
    result$metadata$historical_limitations <- paste(
      "Native params and OOF files contain no historical shared-input/fold MD5 attestation.",
      "Current prep, model predictions and development tables agree; historical CV training was not replayed."
    )
    TRUE
  }, error = function(error) conditionMessage(error))
  result$verified <- isTRUE(checked)
  result$reasons <- if (isTRUE(checked)) character() else paste("Native XGBoost artifact check failed:", checked)
  result
}
})
# END EMBEDDED SOURCE: R/p4_xgboost_artifacts.R

# BEGIN EMBEDDED SOURCE: R/p4_artifact_checks.R
.submission_modules[["R/p4_artifact_checks.R"]] <- quote({
# Read-only consistency/provenance checks before generating the P4 report.
# Source R/p4_helpers.R first. No model is fitted and no artifact is overwritten.

p4_check_artifacts <- function(shared, params, comparison, robustness,
                               interpretation = NULL, threshold_sensitivity = NULL) {
  checks <- list()
  fail <- function(message, action = NULL) {
    stop(paste0("Stale or inconsistent P4 artifacts: ", message,
                if (!is.null(action)) paste0(" ", action)), call. = FALSE)
  }
  passed <- function(check, details, status = "passed") {
    checks[[length(checks) + 1L]] <<- data.frame(
      check = check, status = status, details = details, stringsAsFactors = FALSE)
  }
  near <- function(a, b) isTRUE(all.equal(unname(a), unname(b),
                                         tolerance = 1e-12, check.attributes = FALSE))
  same_ids <- function(a, b) identical(as.character(a), as.character(b))
  hashes <- function(values, artifact, action, required = character()) {
    required <- .submission_input_paths(required)
    if (!is.character(values) || !length(values) || is.null(names(values)) ||
        anyNA(values) || anyNA(names(values)) || any(!nzchar(names(values))) ||
        anyDuplicated(names(values)) || any(!grepl("^[0-9a-fA-F]{32}$", values))) {
      fail(paste0(artifact, " has no valid named input_md5 provenance."), action)
    }
    if (!all(required %in% names(values))) {
      fail(paste0(artifact, " provenance omits required inputs: ",
                  paste(setdiff(required, names(values)), collapse = ", "), "."), action)
    }
    missing <- names(values)[!file.exists(names(values))]
    if (length(missing)) {
      fail(paste0(artifact, " input files are missing: ", paste(missing, collapse = ", "), "."), action)
    }
    current <- tools::md5sum(names(values))
    changed <- names(values)[is.na(current) | tolower(unname(current)) != tolower(unname(values))]
    if (length(changed)) {
      fail(paste0(artifact, " input provenance hash mismatch: ",
                  paste(changed, collapse = ", "), "."), action)
    }
    passed(paste0(artifact, "_input_hashes"),
           paste(length(values), "recorded input hashes match the current files."))
  }
  require_fields <- function(object, fields, artifact, action) {
    if (!is.list(object) || !all(fields %in% names(object))) {
      fail(paste0(artifact, " is missing fields: ",
                  paste(setdiff(fields, names(object)), collapse = ", "), "."), action)
    }
  }
  core_params <- c("threshold", "ntree", "mtry", "nodesize", "seed", "opts")
  same_params <- function(other, artifact, action) {
    require_fields(other, core_params, artifact, action)
    for (field in core_params) {
      if (!identical(other[[field]], params[[field]])) {
        fail(paste0(artifact, " parameter '", field, "' differs from the current RF parameters."), action)
      }
    }
  }
  helpers <- c("p4_read_predictions", "p4_metrics", "prep_apply")
  if (any(!vapply(helpers, function(name) exists(name, envir = .submission_shared, mode = "function", inherits = TRUE), logical(1)))) {
    fail("Required P4/preprocessing helpers are not loaded.", "Source R/p4_helpers.R first.")
  }
  require_fields(shared, c("train", "test", "folds"), "Shared data", "Load the P1 shared data.")
  require_fields(params, core_params, "RF parameters", "Restore the matching RF parameter artifact.")
  require_fields(comparison, c("locked_test_ids", "availability"), "Comparison bundle",
                 "Rerun R/10_model_comparison.R.")
  if (!same_ids(comparison$locked_test_ids, shared$test$obs_id)) {
    fail("Comparison locked test IDs differ from the current shared test IDs.",
         "Rerun R/10_model_comparison.R on the shared split.")
  }
  if (anyDuplicated(shared$train$obs_id) || anyDuplicated(shared$test$obs_id) ||
      any(shared$train$obs_id %in% shared$test$obs_id) ||
      !same_ids(shared$folds$obs_id, shared$train$obs_id)) {
    fail("Shared training/test IDs or CV row alignment are invalid.", "Check the P1 shared split.")
  }
  passed("locked_test_ids", "Comparison test IDs preserve the current locked-test order.")

  # A previously pending prediction file becoming available is also a stale
  # comparison, even though that file could not appear in its old hash vector.
  expected_predictions <- c(
    "Logistic Regression" = "outputs/predictions/logistic_predictions.csv",
    "Random Forest" = "outputs/predictions/random_forest_predictions.csv",
    "XGBoost" = "outputs/predictions/xgboost_predictions.csv")
  availability <- comparison$availability
  if (!is.data.frame(availability) ||
      !all(c("model", "status", "predictions_file") %in% names(availability)) ||
      anyDuplicated(availability$model) ||
      !setequal(availability$model, names(expected_predictions))) {
    fail("Comparison model-availability metadata is invalid.", "Rerun R/10_model_comparison.R.")
  }
  availability <- availability[match(names(expected_predictions), availability$model), , drop = FALSE]
  if (anyNA(availability$status) || !all(availability$status %in% c("available", "pending")) ||
      !same_ids(availability$predictions_file, unname(expected_predictions)) ||
      any(file.exists(unname(expected_predictions)) != (availability$status == "available"))) {
    fail("Prediction-file availability changed since the comparison was produced.",
         "Rerun R/10_model_comparison.R before regenerating the report.")
  }
  hashes(comparison$input_md5, "Comparison", "Rerun R/10_model_comparison.R.",
         required = c("data/processed/test_raw.rds",
                      unname(expected_predictions[availability$status == "available"])))
  passed("model_availability", "Available/pending model rows match the files currently present.")
  if (!is.null(comparison$readiness)) {
    readiness <- comparison$readiness
    flags <- c("run_metadata_available", "model_available", "params_available")
    if (!is.data.frame(readiness) || !all(c("model", flags) %in% names(readiness)) ||
        anyDuplicated(readiness$model) || !setequal(readiness$model, names(expected_predictions))) {
      fail("Comparison readiness metadata is invalid.", "Rerun R/10_model_comparison.R.")
    }
    readiness <- readiness[match(names(expected_predictions), readiness$model), , drop = FALSE]
    prefixes <- c("logistic", "random_forest", "xgboost")
    suffixes <- c("_run_metadata.rds", "_model.rds", "_params.rds")
    for (i in seq_along(prefixes)) {
      for (j in seq_along(flags)) {
        recorded <- readiness[[flags[j]]][i]
        path <- file.path("outputs/models", paste0(prefixes[i], suffixes[j]))
        if (!is.logical(recorded) || length(recorded) != 1L || is.na(recorded) ||
            file.exists(path) != recorded) {
          fail(paste0("Comparison readiness availability changed for ", path, "."),
               "Rerun R/10_model_comparison.R before regenerating the report.")
        }
      }
    }
    passed("provider_metadata_availability", "Provider metadata, saved-model and parameter-file presence still match readiness.")
  } else {
    passed("provider_metadata_availability", "Readiness metadata was not recorded; do not claim complete model handoffs.",
           status = "not_available")
  }

  # The original run manifest protects RF/P1 inputs only. An external model's
  # new prediction export must be validated by a fresh comparison, not rejected
  # merely because its hash differs from the original P4 run's manifest.
  rf_inputs <- c("data/processed/train_raw.rds", "data/processed/test_raw.rds",
                 "data/processed/cv_folds.rds", "data/processed/feature_dictionary.csv")
  original_manifest <- "outputs/tables/p4_input_manifest.csv"
  if (file.exists(original_manifest)) {
    manifest <- read.csv(original_manifest, stringsAsFactors = FALSE)
    if (!all(c("file", "md5") %in% names(manifest)) || anyDuplicated(manifest$file) ||
        !all(rf_inputs %in% manifest$file)) {
      fail("The original P4 RF-input manifest is malformed or incomplete.",
           "Check the original RF data handoff before reusing the fitted model.")
    }
    relevant <- manifest[match(rf_inputs, manifest$file), , drop = FALSE]
    hashes(stats::setNames(relevant$md5, relevant$file), "Original_RF_inputs",
           "RF/P1 training inputs changed; restore the original split or rerun RF and dependent analyses.")
  } else {
    passed("original_RF_manifest", "Original run manifest is absent; remaining artifact checks still apply.",
           status = "not_available")
  }

  require_fields(robustness, c("training_ids", "fold_id", "params", "input_md5",
                                "seeds", "seed_metrics"), "Robustness bundle",
                 "Rerun R/11_robustness_analysis.R after verifying RF outputs.")
  if (!same_ids(robustness$training_ids, shared$train$obs_id)) {
    fail("Robustness training IDs differ from the current shared training IDs.",
         "Rerun R/11_robustness_analysis.R with the matching RF outputs.")
  }
  expected_fold <- integer(nrow(shared$train))
  for (k in seq_len(shared$folds$k)) {
    assessment <- shared$folds$indexOut[[k]]
    if (!setequal(shared$folds$index[[k]], setdiff(seq_len(nrow(shared$train)), assessment)) ||
        anyDuplicated(shared$folds$index[[k]])) {
      fail("A shared CV training fold is not its validation fold's complement.", "Check the P1 folds.")
    }
    expected_fold[assessment] <- k
  }
  if (any(expected_fold == 0L) || anyDuplicated(unlist(shared$folds$indexOut)) ||
      !identical(as.integer(robustness$fold_id), expected_fold)) {
    fail("Robustness fold IDs differ from the current shared CV folds.",
         "Rerun R/11_robustness_analysis.R with the matching RF outputs.")
  }
  same_params(robustness$params, "Robustness", "Rerun R/11_robustness_analysis.R.")
  hashes(robustness$input_md5, "Robustness", "Rerun R/11_robustness_analysis.R.",
         required = c("data/processed/train_raw.rds", "data/processed/cv_folds.rds",
                      "outputs/models/random_forest_params.rds",
                      "outputs/predictions/random_forest_oof_predictions.csv"))
  passed("robustness_configuration", "Training IDs, CV folds and frozen RF parameters match.")

  # This is a prediction-only round trip from the persisted model/preprocessing.
  # No RF training is required to prove that its current export belongs to it.
  model_path <- "outputs/models/random_forest_model.rds"
  if (!file.exists(model_path)) fail("Saved RF model is missing.", "Restore the RF model artifact.")
  bundle <- readRDS(model_path)
  require_fields(bundle, c("model", "prep", "params"), "RF model bundle", "Restore the RF model artifact.")
  require_fields(bundle$prep, c("options", "n_train", "medians", "local_mode", "class_counts", "numeric_vars"),
                 "RF preprocessing", "Restore the matching RF model/preprocessing artifact.")
  require_fields(bundle$model, c("p4_predictors", "ntree", "mtry", "p4_seed", "p4_effective_mtry"),
                 "RF forest", "Restore the matching RF model artifact.")
  same_params(bundle$params, "RF model bundle", "Restore the matching RF model/parameter artifacts.")
  if (!identical(bundle$params, params)) {
    fail("RF model bundle metadata differs from its standalone parameter artifact.",
         "Restore the matching RF model/parameter artifacts.")
  }
  if (!inherits(bundle$prep, "ad_prep") || !identical(bundle$prep$options, params$opts) ||
      bundle$prep$n_train != nrow(shared$train)) {
    fail("Saved RF preprocessing options or training-row count are inconsistent.",
         "Restore the matching RF model/preprocessing artifact.")
  }
  medians <- vapply(names(bundle$prep$medians), function(v) {
    stats::median(shared$train[[v]], na.rm = TRUE)
  }, numeric(1))
  expected_counts <- table(factor(shared$train$y, levels = 0:1, labels = c("nonad", "ad")))
  local_mode <- as.numeric(names(which.max(table(shared$train$local))))
  if (!near(bundle$prep$medians, medians) ||
      !near(bundle$prep$local_mode, local_mode) ||
      !identical(bundle$prep$class_counts, expected_counts) ||
      !identical(bundle$prep$numeric_vars, params$opts$numeric_vars)) {
    fail("Saved RF preprocessing statistics do not match the current training partition.",
         "Check RF input provenance before reusing the model.")
  }
  transformed <- prep_apply(bundle$prep, shared$test, target = "factor")
  predictors <- setdiff(names(transformed), "y")
  effective_mtry <- max(1L, min(as.integer(params$mtry), length(predictors)))
  if (!identical(bundle$model$p4_predictors, predictors) ||
      any(c("obs_id", "sample_id") %in% predictors) ||
      bundle$model$ntree != params$ntree || bundle$model$mtry != effective_mtry ||
      bundle$model$p4_seed != params$seed ||
      bundle$model$p4_effective_mtry != effective_mtry ||
      (!is.null(params$n_predictors) && params$n_predictors != length(predictors))) {
    fail("Saved RF forest structure/seed/predictor metadata is inconsistent.",
         "Restore the matching RF model/parameter artifacts.")
  }
  if (!requireNamespace("randomForest", quietly = TRUE)) {
    fail("Package randomForest is required for the saved-model prediction check.",
         "Install randomForest; no retraining is needed.")
  }
  probability <- as.numeric(stats::predict(bundle$model,
                                           newdata = transformed[, predictors, drop = FALSE],
                                           type = "prob")[, "ad"])
  exported <- p4_read_predictions("outputs/predictions/random_forest_predictions.csv", shared$test)
  if (!near(probability, exported$probability_ad) ||
      !identical(exported$predicted, as.integer(probability >= params$threshold))) {
    fail("Current RF test predictions do not round-trip from the saved model and threshold.",
         "Restore the matching RF model/prediction export; do not retune on test data.")
  }
  passed("RF_model_prediction_roundtrip", "Saved preprocessing/model reproduce current locked-test probabilities and labels.")

  # Check that the seed-42 OOF reuse and all later seed exports still describe
  # the same training-only experiment rather than a mixture of different runs.
  base_path <- "outputs/predictions/random_forest_oof_predictions.csv"
  base <- p4_read_predictions(base_path, shared$train)
  base_metadata <- read.csv(base_path, stringsAsFactors = FALSE)
  id_column <- if ("sample_id" %in% names(base_metadata)) "sample_id" else "obs_id"
  base_metadata <- base_metadata[match(shared$train$obs_id, base_metadata[[id_column]]), , drop = FALSE]
  if (!"fold" %in% names(base_metadata) || anyNA(base_metadata$fold) ||
      !identical(as.integer(base_metadata$fold), expected_fold) ||
      !identical(base$predicted, as.integer(base$probability_ad >= params$threshold))) {
    fail("RF baseline OOF folds or threshold labels are inconsistent.", "Restore the matching RF OOF export.")
  }
  seed_path <- "outputs/predictions/random_forest_robustness_oof_predictions.csv"
  if (!file.exists(seed_path)) fail("RF robustness OOF export is missing.", "Rerun R/11_robustness_analysis.R.")
  seed_predictions <- read.csv(seed_path, stringsAsFactors = FALSE)
  required_seed <- c("sample_id", "actual", "predicted", "probability_ad", "fold", "seed", "threshold")
  if (!all(required_seed %in% names(seed_predictions)) ||
      !setequal(unique(seed_predictions$seed), robustness$seeds) ||
      nrow(seed_predictions) != nrow(shared$train) * length(robustness$seeds) ||
      anyDuplicated(paste(seed_predictions$seed, seed_predictions$sample_id, sep = ":"))) {
    fail("RF robustness seed OOF export has inconsistent rows, seeds or ID pairs.",
         "Rerun R/11_robustness_analysis.R.")
  }
  metric_names <- c("Accuracy", "Precision", "Recall", "Specificity", "F1", "ROC_AUC", "PR_AUC")
  if (!all(c("seed", "threshold", metric_names) %in% names(robustness$seed_metrics)) ||
      anyDuplicated(robustness$seed_metrics$seed) ||
      !setequal(robustness$seed_metrics$seed, robustness$seeds) ||
      !params$seed %in% robustness$seeds) {
    fail("RF robustness seed-summary metadata is inconsistent.", "Rerun R/11_robustness_analysis.R.")
  }
  for (seed in robustness$seeds) {
    d <- seed_predictions[seed_predictions$seed == seed, , drop = FALSE]
    d <- d[match(shared$train$obs_id, d$sample_id), , drop = FALSE]
    if (anyNA(d[, required_seed]) || !same_ids(d$sample_id, shared$train$obs_id) ||
        !identical(as.integer(d$actual), as.integer(shared$train$y)) ||
        !identical(as.integer(d$fold), expected_fold) || any(d$threshold != params$threshold) ||
        !identical(as.integer(d$predicted), as.integer(d$probability_ad >= params$threshold))) {
      fail(paste0("RF robustness OOF rows/folds/thresholds are inconsistent for seed ", seed, "."),
           "Rerun R/11_robustness_analysis.R.")
    }
    computed <- p4_metrics(d$actual, d$probability_ad, d$predicted)[metric_names]
    recorded <- robustness$seed_metrics[robustness$seed_metrics$seed == seed, , drop = FALSE]
    if (recorded$threshold != params$threshold ||
        !near(computed, unlist(recorded[, metric_names, drop = FALSE]))) {
      fail(paste0("RF robustness saved metrics disagree with OOF predictions for seed ", seed, "."),
           "Rerun R/11_robustness_analysis.R.")
    }
    if (seed == params$seed && !near(d$probability_ad, base$probability_ad)) {
      fail("RF robustness baseline probabilities differ from the reused RF OOF export.",
           "Rerun R/11_robustness_analysis.R.")
    }
  }
  passed("RF_seed_OOF_contract", "Each seed covers the same training IDs/folds; baseline probabilities and saved metrics agree.")

  # P2 context is optional. Its absence cannot block RF results, but must never
  # be reported as if P2 findings had already been incorporated.
  if (is.null(interpretation)) {
    passed("P2_interpretation", "No P2 interpretation bundle supplied; do not claim that P2 findings were incorporated.",
           status = "not_available")
  } else if (identical(interpretation$status, "pending")) {
    if (!is.null(interpretation$geometry) || !is.null(interpretation$keywords)) {
      fail("Pending P2 interpretation unexpectedly contains completed context tables.",
           "Rerun R/13_p4_interpretation.R before claiming P2 context.")
    }
    passed("P2_interpretation", "P2 context is pending; RF results remain usable without claiming P2 findings.",
           status = "pending")
  } else if (identical(interpretation$status, "available")) {
    if (is.null(interpretation$geometry) || is.null(interpretation$keywords)) {
      fail("Available P2 interpretation is missing geometry or keyword context.",
           "Rerun R/13_p4_interpretation.R.")
    }
    p2_inputs <- c("data/processed/ad_clean.rds", "data/processed/feature_dictionary.csv",
                   "outputs/tables/class_distribution.csv", "outputs/tables/geometry_descriptive.csv",
                   "outputs/tables/test_a_geometry.csv", "outputs/tables/test_b_keywords.csv",
                   "outputs/tables/keyword_frequency.csv",
                   "outputs/tables/random_forest_feature_importance.csv")
    hashes(interpretation$input_md5, "P2_interpretation", "Rerun R/13_p4_interpretation.R.",
           required = p2_inputs)
    passed("P2_interpretation", "Available P2 context is bound to current P2 tables and RF importance inputs.")
  } else {
    fail("P2 interpretation has an unknown status.", "Rerun R/13_p4_interpretation.R.")
  }
  if (is.null(threshold_sensitivity)) {
    passed("OOF_threshold_sensitivity", "No cross-model OOF threshold bundle supplied.", status = "not_available")
  } else {
    action <- "Rerun R/14_p4_threshold_sensitivity.R."
    require_fields(threshold_sensitivity, c("status", "models", "summary", "sweep", "input_md5"),
                   "OOF threshold sensitivity", action)
    models <- threshold_sensitivity$models
    if (!is.data.frame(models) || !all(c("model", "status", "owner_threshold", "oof_file", "params_file") %in% names(models)) ||
        anyNA(models$model) || anyNA(models$status) ||
        !all(models$status %in% c("available", "pending")) ||
        anyDuplicated(models$model) ||
        !setequal(models$model, c("Logistic Regression", "Random Forest", "XGBoost")) ||
        any((file.exists(models$oof_file) & file.exists(models$params_file)) != (models$status == "available"))) {
      fail("OOF threshold availability metadata is stale or invalid.", action)
    }
    available <- models[models$status == "available", , drop = FALSE]
    expected_status <- if (all(models$status == "available")) "complete" else "pending"
    if (!nrow(available) || !identical(threshold_sensitivity$status, expected_status)) {
      fail("OOF threshold overall status disagrees with available models.", action)
    }
    hashes(threshold_sensitivity$input_md5, "OOF_threshold_sensitivity", action,
           required = c("data/processed/train_raw.rds", "data/processed/cv_folds.rds",
                         "R/p4_helpers.R", "R/14_p4_threshold_sensitivity.R",
                        available$oof_file, available$params_file))
    metric_names <- names(p4_metrics(shared$train$y, rep(0.5, nrow(shared$train))))
    summary <- threshold_sensitivity$summary
    sweep <- threshold_sensitivity$sweep
    summary_columns <- c("model", "threshold_role", "threshold", "n_training", metric_names)
    sweep_columns <- c("model", "threshold", "is_saved_threshold", metric_names)
    if (!is.data.frame(summary) || !all(summary_columns %in% names(summary)) ||
        anyNA(summary$model) || anyNA(summary$threshold_role) ||
        !setequal(summary$model, available$model) ||
        !all(summary$threshold_role %in% c("saved", "reference_0.50")) ||
        anyDuplicated(paste(summary$model, summary$threshold_role, sep = ":")) ||
        nrow(summary) != 2L * nrow(available)) {
      fail("OOF threshold summary roles/schema are missing, duplicated or inconsistent with available models.", action)
    }
    if (!is.data.frame(sweep) || !all(sweep_columns %in% names(sweep)) ||
        anyNA(sweep$model) || !setequal(sweep$model, available$model) ||
        !is.logical(sweep$is_saved_threshold) || anyNA(sweep$is_saved_threshold)) {
      fail("OOF threshold sweep schema or model rows are invalid.", action)
    }
    for (table in list(summary, sweep)) {
      for (column in c("threshold", metric_names)) {
        if (!is.numeric(table[[column]]) || any(!is.finite(table[[column]]))) {
          fail(paste0("OOF threshold metrics contain invalid values in '", column, "'."), action)
        }
      }
    }
    if (!is.numeric(summary$n_training) || any(!is.finite(summary$n_training)) ||
        any(summary$n_training != nrow(shared$train))) {
      fail("OOF threshold summary training sample sizes are inconsistent.", action)
    }
    for (i in seq_len(nrow(available))) {
      saved <- readRDS(available$params_file[i])
      threshold <- if (available$model[i] == "Random Forest") saved$threshold else saved$best_threshold
      rows <- summary[summary$model == available$model[i] & summary$threshold_role == "saved", , drop = FALSE]
      grid <- seq(0.10, 0.90, by = 0.01)
      if (!is.numeric(threshold) || length(threshold) != 1L || !is.finite(threshold) ||
          min(abs(grid - threshold)) > 1e-8 || !near(available$owner_threshold[i], threshold) ||
          nrow(rows) != 1L || !near(rows$threshold, threshold)) {
        fail("OOF sensitivity changed an owner threshold or training sample size.", action)
      }
      raw <- read.csv(available$oof_file[i], stringsAsFactors = FALSE, check.names = FALSE)
      aliases <- intersect(c("sample_id", "obs_id"), names(raw))
      if (!length(aliases) || !all(c("actual", "probability_ad") %in% names(raw)) ||
          (length(aliases) == 2L && !same_ids(raw$sample_id, raw$obs_id))) {
        fail("OOF threshold source columns or ID aliases are inconsistent.", action)
      }
      ids <- raw[[aliases[1L]]]
      if (anyNA(ids) || anyDuplicated(ids) || nrow(raw) != nrow(shared$train) ||
          !setequal(as.character(ids), as.character(shared$train$obs_id))) {
        fail("OOF threshold source IDs do not match the training partition exactly once.", action)
      }
      raw <- raw[match(as.character(shared$train$obs_id), as.character(ids)), , drop = FALSE]
      if (!identical(as.integer(raw$actual), as.integer(shared$train$y))) {
        fail("OOF threshold source labels disagree with shared training truth.", action)
      }
      owner_labels <- as.integer(raw$probability_ad >= threshold)
      p4_metrics(raw$actual, raw$probability_ad, owner_labels)
      if (("predicted" %in% names(raw) &&
           !identical(as.integer(raw$predicted), owner_labels)) ||
          ("fold" %in% names(raw) &&
           !identical(as.integer(raw$fold), expected_fold))) {
        fail("OOF threshold source labels/folds disagree with the saved owner experiment.", action)
      }
      reference <- summary[summary$model == available$model[i] &
                             summary$threshold_role == "reference_0.50", , drop = FALSE]
      if (nrow(reference) != 1L || !near(reference$threshold, 0.50)) {
        fail("OOF threshold reference summary must have exactly one 0.50 row per model.", action)
      }
      for (row in list(rows, reference)) {
        metrics <- p4_metrics(raw$actual, raw$probability_ad, threshold = row$threshold)
        if (!near(metrics, unlist(row[, metric_names, drop = FALSE]))) {
          fail("OOF threshold summary metrics disagree with the saved training OOF scores.", action)
        }
      }
      grid[which.min(abs(grid - threshold))] <- threshold
      grid[which.min(abs(grid - 0.50))] <- 0.50
      curve <- sweep[sweep$model == available$model[i], , drop = FALSE]
      curve <- curve[order(curve$threshold), , drop = FALSE]
      if (nrow(curve) != length(grid) || anyDuplicated(curve$threshold) ||
          !near(curve$threshold, grid) ||
          !identical(curve$is_saved_threshold, abs(grid - threshold) < 1e-8)) {
        fail("OOF threshold sweep must contain the exact unique grid and saved marker for each available model.", action)
      }
      recomputed <- t(vapply(grid, function(cutoff) {
        p4_metrics(raw$actual, raw$probability_ad, threshold = cutoff)
      }, numeric(length(metric_names))))
      if (!near(as.matrix(curve[, metric_names, drop = FALSE]), recomputed)) {
        fail("OOF threshold sweep metrics disagree with the saved training OOF scores.", action)
      }
    }
    passed("OOF_threshold_sensitivity", "Saved cutoffs, exact grid and all summary/sweep metrics reproduce from hashed training-only OOF inputs.")
  }
  do.call(rbind, checks)
}
})
# END EMBEDDED SOURCE: R/p4_artifact_checks.R

# BEGIN EMBEDDED SOURCE: R/01_data_exploration.R
.submission_modules[["R/01_data_exploration.R"]] <- quote({
# =============================================================================
# 01_data_exploration.R -- Dataset exploration
# Owner: Quang (P1)
#
# Input : data/raw/ad.data (+ ad.names)   or   data/raw/add.csv (Kaggle)
# Output: data/processed/ad_clean.rds        cleaned data (y: ad = 1, nonad = 0)
#         data/processed/feature_dictionary.csv
#         outputs/tables/*.csv, report/tables/*.tex, figures
# =============================================================================

.submission_source("R/utils.R")
check_project_root()

# Team decision switch: remove exact duplicate rows (same 1,558 features AND same
# label) before splitting. Keeping them would let identical images appear in
# both train and test, which inflates test performance.
REMOVE_DUPLICATES <- TRUE

# -----------------------------------------------------------------------------
# 1. Read data and check format
# -----------------------------------------------------------------------------
raw  <- read_ad_raw()
ad   <- raw$data
dict <- raw$dictionary
cat("Source:", raw$source, "| rows:", nrow(ad), "| feature columns:", length(feature_columns(ad)), "\n")
cat("Unparseable tokens (not numeric, not '?'):", raw$unparsed, "\n")

# -----------------------------------------------------------------------------
# 2. Target variable: ad = 1, nonad = 0
# -----------------------------------------------------------------------------
label_clean <- sub("\\.$", "", ad$label_raw)
bad_labels <- sum(!label_clean %in% c("ad", "nonad"))
if (bad_labels > 0) stop(bad_labels, " rows have an unknown label")
ad$y <- as.integer(label_clean == "ad")

# -----------------------------------------------------------------------------
# 3. Variable classification
# -----------------------------------------------------------------------------
group_desc <- c(
  geometry = "Image height, width (pixels) and aspect ratio width/height",
  local    = "1 if the image URL is on the same server as the page",
  url      = "Terms in the URL of the page containing the image",
  origurl  = "Terms in the URL of the image itself",
  ancurl   = "Terms in the anchor (link target) URL",
  alt      = "Terms in the image's alt text",
  caption  = "Terms in the anchor text (caption)"
)
groups <- names(group_desc)
active_rate <- vapply(groups, function(g) {
  cols <- dict$column[dict$group == g]
  if (g == "geometry") return(NA_real_)
  mean(rowSums(ad[, cols, drop = FALSE] == 1, na.rm = TRUE) > 0) * 100
}, numeric(1))
example <- vapply(groups, function(g) {
  if (g == "geometry") return("height, width, aratio")
  if (g == "local") return("local")
  dict$original_name[dict$group == g][1]
}, character(1))

var_class <- data.frame(
  Group = groups,
  Type = c("Numerical (continuous)", "Binary", rep("Binary (term indicator)", 5)),
  Features = as.integer(table(factor(dict$group, levels = groups))),
  Example = example,
  `Rows with >=1 active (%)` = active_rate,
  Description = unname(group_desc),
  check.names = FALSE
)
var_class <- rbind(var_class, data.frame(
  Group = "target", Type = "Categorical (binary label)", Features = 1L,
  Example = "ad / nonad", `Rows with >=1 active (%)` = NA_real_,
  Description = "Class label, recoded y = 1 (ad), y = 0 (nonad)", check.names = FALSE))
print(var_class)
tab_vc <- var_class[, c("Group", "Type", "Features", "Example", "Rows with >=1 active (%)")]
names(tab_vc)[5] <- "Active rows (%)"
save_table(tab_vc, "tab_variable_classification",
           caption = "Classification of the 1,559 variables by feature group (original data). Active rows = share of images in which at least one term of the group is present",
           align = "llrlr")

# -----------------------------------------------------------------------------
# 4. Duplicates
# -----------------------------------------------------------------------------
feat <- feature_columns(ad)
feat_key <- do.call(paste, c(ad[, feat], sep = "|"))
full_key <- paste(feat_key, ad$y, sep = "#")
dup_full <- duplicated(full_key)
n_dup_full <- sum(dup_full)
# feature vectors that occur with BOTH labels (irreducible label noise)
labels_per_key <- tapply(ad$y, feat_key, function(v) length(unique(v)))
conflict_keys <- names(labels_per_key)[labels_per_key > 1]
n_conflict_rows <- sum(feat_key %in% conflict_keys & !dup_full)
cat("Exact duplicate rows (extra copies):", n_dup_full, "\n")
cat("Feature vectors with conflicting labels:", length(conflict_keys), "\n")
dup_by_class <- table(factor(ad$y[dup_full], levels = c(0, 1), labels = c("nonad", "ad")))

# -----------------------------------------------------------------------------
# 5. Invalid values
# -----------------------------------------------------------------------------
bin_cols <- binary_columns(ad)
non_binary <- sum(!is.na(as.matrix(ad[, bin_cols])) & !(as.matrix(ad[, bin_cols]) %in% c(0, 1)))
neg_geom <- sum(as.matrix(ad[, NUMERIC_VARS]) <= 0, na.rm = TRUE)
ok <- complete.cases(ad[, NUMERIC_VARS])
rel_err <- abs(ad$aratio[ok] - ad$width[ok] / ad$height[ok]) / (ad$width[ok] / ad$height[ok])
n_aratio_bad <- sum(rel_err > 0.05)
range_geom <- sapply(ad[, NUMERIC_VARS], range, na.rm = TRUE)

inspection <- data.frame(
  Check = c("Observations (rows)", "Feature columns", "Unparseable tokens",
            "Labels outside {ad, nonad}", "Binary values outside {0, 1}",
            "Non-positive height/width/aratio",
            "aratio differs from width/height by >5%",
            "Exact duplicate rows (extra copies)",
            "  of which nonad / ad",
            "Feature vectors observed with both labels",
            "Rows with at least one missing value"),
  Result = c(format(nrow(ad), big.mark = ","), length(feat), raw$unparsed, bad_labels,
             non_binary, neg_geom, n_aratio_bad,
             sprintf("%d (%.1f%%)", n_dup_full, 100 * n_dup_full / nrow(ad)),
             sprintf("%d / %d", dup_by_class[["nonad"]], dup_by_class[["ad"]]),
             length(conflict_keys),
             sprintf("%d (%.1f%%)", sum(!complete.cases(ad[, feat])),
                     100 * mean(!complete.cases(ad[, feat]))))
)
print(inspection)
save_table(inspection, "tab_initial_inspection",
           caption = "Initial data inspection of the original file (3,279 rows)",
           align = "lr")

# -----------------------------------------------------------------------------
# 6. Missing values (original data)
# -----------------------------------------------------------------------------
miss_vars <- feat[colSums(is.na(ad[, feat])) > 0]
missing_tab <- data.frame(
  Variable = miss_vars,
  Missing = colSums(is.na(ad[, miss_vars])),
  `Missing (%)` = 100 * colMeans(is.na(ad[, miss_vars])),
  check.names = FALSE, row.names = NULL
)
print(missing_tab)
pattern <- apply(is.na(ad[, NUMERIC_VARS]), 1, function(r) {
  if (!any(r)) "none" else paste(NUMERIC_VARS[r], collapse = "+")
})
print(table(pattern))
write.csv(as.data.frame(table(pattern)), file.path(PATHS$out_tab, "missing_patterns_original.csv"),
          row.names = FALSE)

# -----------------------------------------------------------------------------
# 7. Remove duplicates -> cleaned data set
# -----------------------------------------------------------------------------
ad_clean <- if (REMOVE_DUPLICATES) ad[!dup_full, ] else ad
ad_clean$label_raw <- NULL
ad_clean <- ad_clean[, c("obs_id", "y", feat)]
rownames(ad_clean) <- NULL

# missing table after cleaning (used in the report)
missing_tab$`Missing after dedup` <- colSums(is.na(ad_clean[, miss_vars]))
missing_tab$`After dedup (%)` <- 100 * colMeans(is.na(ad_clean[, miss_vars]))
save_table(missing_tab, "tab_missing_values",
           caption = "Missing values by variable, before and after removing duplicate rows",
           align = "lrrrr")

# -----------------------------------------------------------------------------
# 8. Class distribution and imbalance
# -----------------------------------------------------------------------------
class_tab <- function(y, version) {
  n <- table(factor(y, levels = c(0, 1), labels = c("nonad", "ad")))
  data.frame(Version = version, Class = names(n), n = as.integer(n),
             pct = 100 * as.numeric(n) / length(y))
}
cls <- rbind(class_tab(ad$y, "Original (n = 3,279)"),
             class_tab(ad_clean$y, sprintf("Deduplicated (n = %s)",
                                           format(nrow(ad_clean), big.mark = ","))))
print(cls)
ci_raw   <- binom.test(sum(ad$y), nrow(ad))$conf.int * 100
ci_clean <- binom.test(sum(ad_clean$y), nrow(ad_clean))$conf.int * 100
overview <- data.frame(
  Item = c("Source", "Observations (original)", "Observations after removing duplicates",
           "Variables", "  numerical / binary / target",
           "ad (y = 1) / nonad (y = 0), deduplicated",
           "Proportion of ads, deduplicated [95% CI]",
           "Imbalance ratio nonad : ad, deduplicated",
           "Rows with missing geometry, deduplicated"),
  Value = c("UCI ML Repository / Kaggle (Kushmerick, 1998)",
            format(nrow(ad), big.mark = ","), format(nrow(ad_clean), big.mark = ","),
            "1,559", "3 / 1,555 / 1",
            paste(format(sum(ad_clean$y), big.mark = ","), "/",
                  format(sum(ad_clean$y == 0), big.mark = ",")),
            sprintf("%.2f%% [%.2f%%, %.2f%%]", 100 * mean(ad_clean$y), ci_clean[1], ci_clean[2]),
            sprintf("%.2f : 1", sum(ad_clean$y == 0) / sum(ad_clean$y)),
            sprintf("%d (%.1f%%)", sum(!complete.cases(ad_clean[, NUMERIC_VARS])),
                    100 * mean(!complete.cases(ad_clean[, NUMERIC_VARS]))))
)
print(overview)
save_table(overview, "tab_dataset_overview", caption = "Dataset overview", align = "ll")

if (has_ggplot()) {
  library(ggplot2)
  cls$Class <- factor(cls$Class, levels = c("nonad", "ad"))
  cls$Version <- factor(cls$Version, levels = unique(cls$Version))
  p <- ggplot(cls, aes(x = Class, y = n, fill = Class)) +
    geom_col(width = 0.6) +
    geom_text(aes(label = sprintf("%s (%.1f%%)", format(n, big.mark = ","), pct)),
              vjust = -0.4, size = 3.2, colour = "#0b0b0b") +
    facet_wrap(~Version) +
    scale_fill_manual(values = CLASS_COLOURS, guide = "none") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
    labs(x = NULL, y = "Number of images") +
    theme_report()
  save_figure(p, "fig_class_distribution", width = 6, height = 3.2)
}

# -----------------------------------------------------------------------------
# 9. Save
# -----------------------------------------------------------------------------
saveRDS(ad_clean, file.path(PATHS$processed, "ad_clean.rds"))
write.csv(dict, file.path(PATHS$processed, "feature_dictionary.csv"), row.names = FALSE)
cat("\nSaved data/processed/ad_clean.rds:", nrow(ad_clean), "rows x", ncol(ad_clean), "columns\n")
})
# END EMBEDDED SOURCE: R/01_data_exploration.R

# BEGIN EMBEDDED SOURCE: R/02_data_preprocessing.R
.submission_modules[["R/02_data_preprocessing.R"]] <- quote({
# =============================================================================
# 02_data_preprocessing.R -- Preprocessing diagnostics and decisions
# Owner: Quang (P1)
#
# This script DIAGNOSES the data to justify each preprocessing decision
# (missing values, outliers, near-zero-variance features, scaling, imbalance).
# It does not learn any parameter used by the models: all learned quantities
# (medians, kept features, scaling) are estimated on training data only by
# prep_fit() in R/preprocessing_pipeline.R (see 03_data_split.R).
#
# Input : data/processed/ad_clean.rds (from 01_data_exploration.R)
# Output: tables + figures for Section 3 of the report
# =============================================================================

.submission_source("R/utils.R")
.submission_source("R/preprocessing_pipeline.R")
check_project_root()

ad <- readRDS(file.path(PATHS$processed, "ad_clean.rds"))
cls <- as_class_factor(ad$y)
bin <- binary_columns(ad)

# -----------------------------------------------------------------------------
# 1. Missing values: is missingness related to the label? (MCAR check)
# -----------------------------------------------------------------------------
ad$geom_missing <- as.integer(rowSums(is.na(ad[, NUMERIC_VARS])) > 0)
miss_by_class <- tapply(ad$geom_missing, cls, mean)
tab_gm <- table(Missing = ad$geom_missing, Class = cls)
chi <- chisq.test(tab_gm)
pt  <- prop.test(x = c(sum(ad$geom_missing[ad$y == 1]), sum(ad$geom_missing[ad$y == 0])),
                 n = c(sum(ad$y == 1), sum(ad$y == 0)))
cat(sprintf("Geometry missing: ad %.1f%%, nonad %.1f%%; chi2 = %.2f, df = %d, p = %.4f\n",
            100 * miss_by_class["ad"], 100 * miss_by_class["nonad"],
            chi$statistic, chi$parameter, chi$p.value))
cat(sprintf("Difference ad - nonad: %.1f pp, 95%% CI [%.1f, %.1f]\n",
            100 * diff(rev(pt$estimate)), 100 * pt$conf.int[1], 100 * pt$conf.int[2]))

miss_class_tab <- data.frame(
  Variable = c(NUMERIC_VARS, "local", "any geometry"),
  `Missing (n)` = c(colSums(is.na(ad[, c(NUMERIC_VARS, "local")])), sum(ad$geom_missing)),
  `nonad (%)` = c(sapply(c(NUMERIC_VARS, "local"), function(v) 100 * mean(is.na(ad[[v]][ad$y == 0]))),
                  100 * miss_by_class[["nonad"]]),
  `ad (%)` = c(sapply(c(NUMERIC_VARS, "local"), function(v) 100 * mean(is.na(ad[[v]][ad$y == 1]))),
               100 * miss_by_class[["ad"]]),
  check.names = FALSE
)
p_fmt <- function(p) if (p < 0.001) "< 0.001" else sprintf("%.3f", p)
miss_class_tab$`p-value` <- c(sapply(c(NUMERIC_VARS, "local"), function(v) {
  p_fmt(fisher.test(table(is.na(ad[[v]]), cls))$p.value)
}), p_fmt(chi$p.value))
print(miss_class_tab)
save_table(miss_class_tab, "tab_missing_by_class",
           caption = "Missing-value rates by class (deduplicated data). $p$-values: Fisher's exact test per variable, Pearson $\\chi^2$ test for any missing geometry",
           align = "lrrrr")

# Compare candidate strategies (descriptive; the final values are learned on training data)
h_obs <- ad$height[!is.na(ad$height)]
h_imp <- ifelse(is.na(ad$height), median(h_obs), ad$height)
cc <- complete.cases(ad[, c(NUMERIC_VARS, "local")])
strategies <- data.frame(
  Strategy = c("Complete-case deletion", "Median imputation",
               "Median + missingness indicator"),
  `Rows kept` = c(sum(cc), nrow(ad), nrow(ad)),
  `Ads kept` = c(sum(ad$y[cc]), sum(ad$y), sum(ad$y)),
  `Ad share (%)` = c(100 * mean(ad$y[cc]), 100 * mean(ad$y), 100 * mean(ad$y)),
  `SD of height` = c(sd(ad$height[cc]), sd(h_imp), sd(h_imp)),
  `Keeps NA info` = c("No", "No", "Yes"),
  check.names = FALSE
)
print(strategies)
save_table(strategies, "tab_missing_strategies",
           caption = sprintf("Comparison of missing-value strategies (deduplicated data; SD of observed height = %.1f)", sd(h_obs)),
           align = "lrrrrl", size = "\\footnotesize")

if (has_ggplot()) {
  library(ggplot2)
  mdf <- data.frame(Class = factor(c("nonad", "ad"), levels = c("nonad", "ad")),
                    n = c(sum(ad$y == 0), sum(ad$y == 1)),
                    k = c(sum(ad$geom_missing[ad$y == 0]), sum(ad$geom_missing[ad$y == 1])))
  ci <- t(mapply(function(k, n) binom.test(k, n)$conf.int, mdf$k, mdf$n))
  mdf$p <- mdf$k / mdf$n; mdf$lo <- ci[, 1]; mdf$hi <- ci[, 2]
  p <- ggplot(mdf, aes(x = Class, y = 100 * p, fill = Class)) +
    geom_col(width = 0.55) +
    geom_errorbar(aes(ymin = 100 * lo, ymax = 100 * hi), width = 0.12, linewidth = 0.4,
                  colour = "#52514e") +
    geom_text(aes(y = 100 * hi, label = sprintf("%.1f%%", 100 * p)), vjust = -0.6,
              size = 3.2, colour = "#0b0b0b") +
    scale_fill_manual(values = CLASS_COLOURS, guide = "none") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    labs(x = NULL, y = "Rows with missing geometry (%)") +
    theme_report()
  save_figure(p, "fig_missing_by_class", width = 3.6, height = 3)
}

# -----------------------------------------------------------------------------
# 2. Outliers in height, width, aratio (IQR rule, raw and log scale)
# -----------------------------------------------------------------------------
skew <- function(x) { x <- x[!is.na(x)]; mean((x - mean(x))^3) / sd(x)^3 }
iqr_info <- function(x, y) {
  ok <- !is.na(x); x <- x[ok]; y <- y[ok]
  q <- quantile(x, c(0.25, 0.75)); iqr <- diff(q)
  lo <- q[1] - 1.5 * iqr; hi <- q[2] + 1.5 * iqr
  out <- x < lo | x > hi
  c(Q1 = q[[1]], Median = median(x), Q3 = q[[2]], Upper = hi[[1]], Max = max(x),
    Outliers = sum(out), `Outliers (%)` = 100 * mean(out),
    `Ad share in outliers (%)` = if (any(out)) 100 * mean(y[out]) else NA)
}
out_tab <- do.call(rbind, lapply(NUMERIC_VARS, function(v) iqr_info(ad[[v]], ad$y)))
out_tab <- data.frame(Variable = NUMERIC_VARS, out_tab, check.names = FALSE)
out_tab$Skewness <- sapply(NUMERIC_VARS, function(v) skew(ad[[v]]))
out_tab$`Outliers after log` <- sapply(NUMERIC_VARS, function(v) iqr_info(log(ad[[v]]), ad$y)[["Outliers"]])
out_tab$`Skewness after log` <- sapply(NUMERIC_VARS, function(v) skew(log(ad[[v]])))
print(out_tab)
out_print <- out_tab[, c("Variable", "Q1", "Median", "Q3", "Upper", "Max", "Outliers",
                         "Outliers (%)", "Ad share in outliers (%)", "Skewness",
                         "Outliers after log", "Skewness after log")]
names(out_print) <- c("Variable", "$Q_1$", "Median", "$Q_3$", "Upper fence", "Max", "Out.",
                      "Out. (\\%)", "Ad in out. (\\%)", "Skew", "Out. (log)", "Skew (log)")
out_print$Variable <- paste0("\\texttt{", out_print$Variable, "}")
save_table(out_print, "tab_outliers",
           caption = sprintf("IQR outlier analysis of the geometry features (observed values; overall ad share = %.1f\\%%). Upper fence $= Q_3 + 1.5\\,\\mathrm{IQR}$; no value lies below the lower fence. ``Out.'' = number of outliers; ``(log)'' = the same analysis after a log transform",
                             100 * mean(ad$y)),
           align = "lrrrrrrrrrrr", escape = FALSE, size = "\\footnotesize", fit_width = TRUE)

# Validity checks: are the extreme values physically possible?
validity <- data.frame(
  Check = c("height or width outside [1, 640] px",
            "aratio inconsistent with width/height (>5%)",
            "Images with width = 468 px (standard banner width)",
            "  ad share among them (%)"),
  Value = c(sum(ad$height < 1 | ad$height > 640 | ad$width < 1 | ad$width > 640, na.rm = TRUE),
            sum(abs(ad$aratio - ad$width / ad$height) / (ad$width / ad$height) > 0.05, na.rm = TRUE),
            sum(ad$width == 468, na.rm = TRUE),
            sprintf("%.1f", 100 * mean(ad$y[which(ad$width == 468)])))
)
print(validity)
write.csv(validity, file.path(PATHS$out_tab, "outlier_validity_checks.csv"), row.names = FALSE)

if (has_ggplot()) {
  long <- do.call(rbind, lapply(NUMERIC_VARS, function(v)
    data.frame(Variable = v, value = ad[[v]], Class = cls)))
  long <- long[!is.na(long$value), ]
  long$Variable <- factor(long$Variable, levels = NUMERIC_VARS)
  long$Class <- factor(long$Class, levels = c("nonad", "ad"))
  p <- ggplot(long, aes(x = Class, y = value, fill = Class)) +
    geom_boxplot(width = 0.55, outlier.size = 0.6, outlier.alpha = 0.5,
                 linewidth = 0.35, colour = "#52514e") +
    facet_wrap(~Variable, scales = "free_y") +
    scale_y_log10(labels = function(x) format(x, scientific = FALSE, drop0trailing = TRUE,
                                              trim = TRUE)) +
    scale_fill_manual(values = CLASS_COLOURS) +
    labs(x = NULL, y = "Value (log scale)") +
    theme_report() + theme(legend.position = "none")
  save_figure(p, "fig_boxplots_geometry", width = 6.2, height = 3)
}

# -----------------------------------------------------------------------------
# 3. Zero / near-zero variance and redundant binary features
#    (descriptive on all rows; the actual filter is learned on training data)
# -----------------------------------------------------------------------------
B <- as.matrix(ad[, bin]); B[is.na(B)] <- 0
ones <- colSums(B)
minority <- pmin(ones, nrow(B) - ones)
bins <- cut(minority, c(-Inf, 0, 1, 2, 4, 9, 19, 49, Inf),
            labels = c("0", "1", "2", "3-4", "5-9", "10-19", "20-49", ">=50"))
sparsity <- as.data.frame(table(`Minority count` = bins))
names(sparsity) <- c("Minority count", "Features")
sparsity$`Cumulative (%)` <- 100 * cumsum(sparsity$Features) / length(bin)
print(sparsity)
n_dup_cols <- sum(duplicated(t(B)))
cat("Binary features:", length(bin), "| prevalence < 1%:", sum(ones / nrow(B) < 0.01),
    "| duplicate columns:", n_dup_cols, "\n")
write.csv(sparsity, file.path(PATHS$out_tab, "binary_sparsity.csv"), row.names = FALSE)

if (has_ggplot()) {
  sdf <- data.frame(ones = ones)
  p <- ggplot(sdf, aes(x = ones)) +
    geom_histogram(bins = 40, fill = CLASS_COLOURS[["nonad"]], colour = "white", linewidth = 0.2) +
    geom_vline(xintercept = 5, linetype = "dashed", linewidth = 0.4, colour = "#52514e") +
    annotate("text", x = 4.6, y = Inf, label = "min_count = 5", hjust = 1, vjust = 1.5,
             size = 3, colour = "#52514e") +
    scale_x_log10() +
    labs(x = "Number of images in which the term is present (log scale)",
         y = "Number of binary features") +
    theme_report()
  save_figure(p, "fig_binary_sparsity", width = 6, height = 3)
}

# -----------------------------------------------------------------------------
# 4. Class imbalance: illustrative weights (final weights come from training data)
# -----------------------------------------------------------------------------
cat("Balanced class weights (all deduplicated rows):\n"); print(class_weights(ad$y))
cat("XGBoost scale_pos_weight:", round(scale_pos_weight(ad$y), 3), "\n")

# Key numbers for the report text
key <- c(
  chi2 = unname(chi$statistic), p_chi2 = chi$p.value,
  miss_ad = 100 * miss_by_class[["ad"]], miss_nonad = 100 * miss_by_class[["nonad"]],
  diff_lo = 100 * pt$conf.int[1], diff_hi = 100 * pt$conf.int[2],
  dup_cols = n_dup_cols, prevalence_below_1pct = sum(ones / nrow(B) < 0.01)
)
write.csv(data.frame(key = names(key), value = key), file.path(PATHS$out_tab, "key_numbers_02.csv"),
          row.names = FALSE)
})
# END EMBEDDED SOURCE: R/02_data_preprocessing.R

# BEGIN EMBEDDED SOURCE: R/03_data_split.R
.submission_modules[["R/03_data_split.R"]] <- quote({
# =============================================================================
# 03_data_split.R -- Stratified train/test split, shared CV folds,
#                    and verification of the shared preprocessing pipeline
# Owner: Quang (P1)
#
# Input : data/processed/ad_clean.rds
# Output: data/processed/train_raw.rds   80% (not preprocessed; NA kept)
#         data/processed/test_raw.rds    20% (not preprocessed; NA kept)
#         data/processed/cv_folds.rds    5 stratified folds on the training set
#         data/processed/split_indices.csv  obs_id -> set / fold (human-readable)
# =============================================================================

.submission_source("R/utils.R")
.submission_source("R/preprocessing_pipeline.R")
check_project_root()

TRAIN_PROP <- 0.80
N_FOLDS    <- 5L

ad <- readRDS(file.path(PATHS$processed, "ad_clean.rds"))

# -----------------------------------------------------------------------------
# Stratified sampling helpers (base R, deterministic given the seed)
# -----------------------------------------------------------------------------
stratified_split <- function(y, prop, seed = PROJECT_SEED) {
  set_project_seed(seed)
  idx <- unlist(lapply(split(seq_along(y), y), function(ix) {
    ix[sample.int(length(ix), round(prop * length(ix)))]
  }), use.names = FALSE)
  sort(idx)
}

stratified_folds <- function(y, k, seed = PROJECT_SEED) {
  set_project_seed(seed)
  fold <- integer(length(y))
  for (ix in split(seq_along(y), y)) {
    fold[ix[sample.int(length(ix))]] <- rep_len(seq_len(k), length(ix))
  }
  fold
}

# -----------------------------------------------------------------------------
# 1. Train / test split (80/20, stratified on y, seed 42)
# -----------------------------------------------------------------------------
train_idx <- stratified_split(ad$y, TRAIN_PROP)
train <- ad[train_idx, ];  rownames(train) <- NULL
test  <- ad[-train_idx, ]; rownames(test)  <- NULL

# -----------------------------------------------------------------------------
# 2. Shared 5-fold stratified CV on the training set
#    index[[k]]    = training-row positions used to FIT in fold k   (caret: index)
#    indexOut[[k]] = training-row positions used to VALIDATE fold k (caret: indexOut,
#                    xgboost::xgb.cv(folds = indexOut))
# -----------------------------------------------------------------------------
fold_id <- stratified_folds(train$y, N_FOLDS)
folds <- list(
  k        = N_FOLDS,
  seed     = PROJECT_SEED,
  fold_id  = fold_id,
  obs_id   = train$obs_id,
  index    = lapply(seq_len(N_FOLDS), function(k) which(fold_id != k)),
  indexOut = lapply(seq_len(N_FOLDS), function(k) which(fold_id == k))
)
names(folds$index) <- names(folds$indexOut) <- sprintf("Fold%d", seq_len(N_FOLDS))

# -----------------------------------------------------------------------------
# 3. Checks: reproducibility, no overlap, stratification
# -----------------------------------------------------------------------------
stopifnot(
  identical(train_idx, stratified_split(ad$y, TRAIN_PROP)),     # same seed -> same split
  identical(fold_id, stratified_folds(train$y, N_FOLDS)),
  length(intersect(train$obs_id, test$obs_id)) == 0,
  nrow(train) + nrow(test) == nrow(ad),
  all(sort(unlist(folds$indexOut)) == seq_len(nrow(train)))
)
feat <- feature_columns(ad)
key <- function(d) do.call(paste, c(d[, feat], sep = "|"))
shared_vectors <- sum(key(test) %in% key(train))
cat("Test rows whose feature vector also appears in train:", shared_vectors, "\n")

summ <- function(y, name) data.frame(Set = name, n = length(y), nonad = sum(y == 0),
                                     ad = sum(y == 1), `Ad (%)` = 100 * mean(y),
                                     check.names = FALSE)
split_tab <- rbind(
  summ(ad$y, "Full (deduplicated)"), summ(train$y, "Training (80%)"), summ(test$y, "Test (20%)"),
  do.call(rbind, lapply(seq_len(N_FOLDS), function(k)
    summ(train$y[folds$indexOut[[k]]], sprintf("  CV fold %d (validation)", k))))
)
print(split_tab)
save_table(split_tab, "tab_split_summary",
           caption = "Stratified train/test split (seed 42) and the shared 5-fold cross-validation folds",
           align = "lrrrr")

# -----------------------------------------------------------------------------
# 4. Fit the shared pipeline on the training set for each model preset
#    (illustration + sanity checks; each modeller refits inside every CV fold)
# -----------------------------------------------------------------------------
presets <- c("logistic", "random_forest", "xgboost")
preps <- lapply(presets, function(m) prep_fit(train, prep_options(m)))
names(preps) <- presets
for (m in presets) { print(preps[[m]]) }

feat_tab <- do.call(rbind, lapply(presets, function(m) {
  p <- preps[[m]]
  tr <- prep_apply(p, train); te <- prep_apply(p, test)
  stopifnot(identical(names(tr), names(te)))                 # same columns in train/test
  if (m != "xgboost") stopifnot(!anyNA(tr), !anyNA(te))      # imputed presets: no NA left
  data.frame(Preset = m,
             `Binary` = length(binary_columns(train)),
             `Zero var.` = length(p$dropped$zero_variance),
             `Rare ($<5$)` = length(p$dropped$rare),
             `Dupl.` = length(p$dropped$duplicate),
             `Kept` = length(p$binary_keep),
             `Numeric` = length(p$numeric_vars) + isTRUE(p$options$missing_indicator),
             `Total` = ncol(tr) - 1,
             check.names = FALSE)
}))
feat_tab$Preset <- c("Logistic Regression", "Random Forest", "XGBoost")
print(feat_tab)
save_table(feat_tab, "tab_feature_filtering",
           caption = sprintf("Feature filtering learned on the training set ($n = %s$). Numeric = continuous predictors + \\texttt{geom\\_missing}",
                             format(nrow(train), big.mark = "{,}")),
           align = "lrrrrrrr", escape = FALSE)

# Number of retained features in each CV fold (pipeline re-fitted per fold)
fold_feats <- sapply(seq_len(N_FOLDS), function(k)
  length(prep_fit(train[folds$index[[k]], ], prep_options("random_forest"))$binary_keep))
cat("Binary features kept per CV fold:", fold_feats, "\n")

# Leakage check: imputation medians equal the TRAINING medians, not the full-data ones
med_train <- sapply(NUMERIC_VARS, function(v) median(train[[v]], na.rm = TRUE))
med_full  <- sapply(NUMERIC_VARS, function(v) median(ad[[v]], na.rm = TRUE))
stopifnot(isTRUE(all.equal(preps$random_forest$medians, med_train)))
cat("Training medians:", med_train, "| full-data medians (not used):", med_full, "\n")

# Class weights from the TRAINING set
cw <- class_weights(train$y)
cw_tab <- data.frame(
  Quantity = c("Training nonad / ad", "Class weight nonad, $n/(2n_0)$",
               "Class weight ad, $n/(2n_1)$", "XGBoost \\texttt{scale\\_pos\\_weight} $= n_0/n_1$"),
  Value = c(sprintf("%d / %d", sum(train$y == 0), sum(train$y == 1)),
            sprintf("%.3f", cw[["0"]]), sprintf("%.3f", cw[["1"]]),
            sprintf("%.3f", scale_pos_weight(train$y)))
)
print(cw_tab)
save_table(cw_tab, "tab_class_weights", caption = "Class-imbalance weights computed on the training set",
           align = "lr", escape = FALSE)

# -----------------------------------------------------------------------------
# 5. Save shared artefacts
# -----------------------------------------------------------------------------
saveRDS(train, file.path(PATHS$processed, "train_raw.rds"))
saveRDS(test,  file.path(PATHS$processed, "test_raw.rds"))
saveRDS(folds, file.path(PATHS$processed, "cv_folds.rds"))
split_csv <- data.frame(obs_id = c(train$obs_id, test$obs_id),
                        set = rep(c("train", "test"), c(nrow(train), nrow(test))),
                        fold = c(fold_id, rep(NA, nrow(test))),
                        y = c(train$y, test$y))
split_csv <- split_csv[order(split_csv$obs_id), ]
write.csv(split_csv, file.path(PATHS$processed, "split_indices.csv"), row.names = FALSE)

cat("\nSaved train_raw.rds (", nrow(train), " rows), test_raw.rds (", nrow(test),
    " rows), cv_folds.rds (", N_FOLDS, " folds), split_indices.csv\n", sep = "")
})
# END EMBEDDED SOURCE: R/03_data_split.R

# BEGIN EMBEDDED SOURCE: R/04_descriptive_statistics.R
.submission_modules[["R/04_descriptive_statistics.R"]] <- quote({
# =============================================================================
# 04_descriptive_statistics.R -- Descriptive Statistics
#
# Descriptive Statistical Analysis
# Input   : data/processed/ad_clean.rds and feature_dictionary.csv
# Output  : CSV tables in outputs/tables/
#           PNG figures in outputs/figures/
#           PNG figures for LaTeX in report/Part_2/figures/
# =============================================================================


.submission_source("R/statistical_analysis_io.R")


# -----------------------------------------------------------------------------
# 1. Count ads and nonads
# -----------------------------------------------------------------------------
# Divide each class count by the total number of rows after removing duplicates.
class_counts <- table(factor(ads$class, levels = c("ad", "nonad")))
counts <- data.frame(Class = names(class_counts), N = as.integer(class_counts),
                     Share = as.numeric(prop.table(class_counts)))
write_analysis_csv(counts, "class_distribution")


# -----------------------------------------------------------------------------
# 2. Describe height, width and aspect ratio
# -----------------------------------------------------------------------------
# Summarize each feature using its available values, for all rows and by class
# Count values more than 1.5 IQR below Q1 or above Q3. Keep them in the summaries
describe <- function(data, group, feature) {
  x <- data[[feature]]
  valid <- x[!is.na(x)]
  q <- quantile(valid, c(0.25, 0.5, 0.75), names = FALSE)
  spread <- q[3] - q[1]
  data.frame(group, feature, n = length(valid), missing = sum(is.na(x)),
             mean = mean(valid), median = q[2], variance = var(valid),
             sd = sd(valid), min = min(valid), q1 = q[1], q3 = q[3],
             max = max(valid), iqr = spread,
             iqr_outliers = sum(valid < q[1] - 1.5 * spread |
                                valid > q[3] + 1.5 * spread))
}
summary_rows <- do.call(rbind, lapply(names(groups), function(group) {
  do.call(rbind, lapply(geometry, function(feature) describe(groups[[group]], group, feature)))
}))
write_analysis_csv(summary_rows, "geometry_descriptive")


# -----------------------------------------------------------------------------
# 3. Count missing values and keyword occurrences
# -----------------------------------------------------------------------------
# Missing rate = number of missing values / total rows in the group
missing <- do.call(rbind, lapply(names(groups), function(group) {
  data <- groups[[group]][features]
  n_missing <- vapply(data, function(x) sum(is.na(x)), integer(1))
  data.frame(group, feature = features, n = nrow(data), missing = n_missing,
             fraction = n_missing / nrow(data))
}))
write_analysis_csv(missing, "missing_by_feature")

# Keyword frequency = number of 1s / number of recorded 0s and 1s
frequency <- do.call(rbind, lapply(names(groups), function(group) {
  data <- groups[[group]][keywords]
  present <- vapply(data, function(x) sum(x == 1, na.rm = TRUE), integer(1))
  observed <- vapply(data, function(x) sum(!is.na(x)), integer(1))
  data.frame(group, feature = keywords, observed, present, share = present / observed)
}))
write_analysis_csv(frequency, "keyword_frequency")


# -----------------------------------------------------------------------------
# 4. Draw the class and geometry plots
# -----------------------------------------------------------------------------
class_labels <- c(ad = "Advertisement", nonad = "Non-advertisement")
save_analysis_figure("fig_descriptive_class_distribution", width = 5, height = 3.4,
                     draw = function() {
  barplot(class_counts, names.arg = class_labels[names(class_counts)],
          col = c("#A33F35", "#30647C"), main = "Distribution of Image Classes",
          xlab = "Image class", ylab = "Number of images",
          ylim = c(0, 1.12 * max(class_counts)))
})

# The plots use log(1 + x) to make the skewed distributions easier to read.
# The tables and statistical tests use the original values.
geometry_labels <- c(height = "Image height (px)", width = "Image width (px)",
                     aratio = "Aspect ratio (width / height)")
save_analysis_figure("fig_geometry_distributions", width = 8, height = 5,
                     draw = function() {
  par(mfrow = c(2, 3), mar = c(4, 3.5, 2, 1), mgp = c(2, 0.6, 0))
  for (feature in geometry) {
    hist(log1p(ads[[feature]]), breaks = "FD", col = "#30647C",
         main = geometry_labels[[feature]], xlab = "Transformed value: log(1 + x)",
         ylab = "Number of images", cex.main = 0.95, cex.lab = 0.9)
  }
  for (feature in geometry) {
    boxplot(log1p(ads[[feature]]) ~ factor(ads$class, levels = c("ad", "nonad")),
            names = class_labels[c("ad", "nonad")], col = c("#A33F35", "#30647C"),
            xlab = "Image class", ylab = "Transformed value: log(1 + x)",
            cex.axis = 0.75, cex.lab = 0.9)
  }
})
})
# END EMBEDDED SOURCE: R/04_descriptive_statistics.R

# BEGIN EMBEDDED SOURCE: R/05_estimation.R
.submission_modules[["R/05_estimation.R"]] <- quote({
# =============================================================================
# 05_estimation.R -- Parameter Estimation
#
# Parameter Estimation (Section 5.1)
# Input   : data/processed/ad_clean.rds and feature_dictionary.csv
# Output  : Estimates and confidence intervals in outputs/tables/
# Execution: called automatically by the analysis.R runner.
# =============================================================================


.submission_source("R/statistical_analysis_io.R")


# -----------------------------------------------------------------------------
# 1. Estimate the ad share
# -----------------------------------------------------------------------------
# k counts ads and n counts all rows. The observed ad share is k/n
# Use the Wilson formula to get the lower and upper bounds of the 95% interval
n <- nrow(ads)
k <- sum(ads$ad)
p <- k / n
z <- qnorm(0.975)
denominator <- 1 + z^2 / n
center <- (p + z^2 / (2 * n)) / denominator
margin <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / denominator
proportion <- data.frame(n, ads = k, estimate = p, lower = center - margin,
                         upper = center + margin, method = "Wilson")
write_analysis_csv(proportion, "ad_proportion_ci")


# -----------------------------------------------------------------------------
# 2. Estimate mean height, width and aspect ratio
# -----------------------------------------------------------------------------
# Compute each mean using the available values in the group
# Read the 95% confidence interval for that mean from t.test(x)
means <- do.call(rbind, lapply(names(groups), function(group) {
  do.call(rbind, lapply(geometry, function(feature) {
    x <- na.omit(groups[[group]][[feature]])
    result <- t.test(x)
    data.frame(group, feature, n = length(x), estimate = mean(x),
               lower = result$conf.int[1], upper = result$conf.int[2])
  }))
}))


# -----------------------------------------------------------------------------
# 3. Estimate the difference between ad and nonad means
# -----------------------------------------------------------------------------
# Subtract the nonad mean from the ad mean. A positive value means ads average higher
# Use Welch's interval because the two classes can have different variances
differences <- do.call(rbind, lapply(geometry, function(feature) {
  x <- na.omit(groups$ad[[feature]])
  y <- na.omit(groups$nonad[[feature]])
  result <- t.test(x, y)
  data.frame(feature, n_ad = length(x), n_nonad = length(y),
             estimate = mean(x) - mean(y), lower = result$conf.int[1],
             upper = result$conf.int[2])
}))


# -----------------------------------------------------------------------------
# 4. Save the estimates and confidence intervals
# -----------------------------------------------------------------------------
# The 95% confidence level applies to each interval individually
write_analysis_csv(means, "geometry_mean_ci")
write_analysis_csv(differences, "geometry_mean_difference_ci")
})
# END EMBEDDED SOURCE: R/05_estimation.R

# BEGIN EMBEDDED SOURCE: R/06_hypothesis_testing.R
.submission_modules[["R/06_hypothesis_testing.R"]] <- quote({
# =============================================================================
# 06_hypothesis_testing.R -- Hypothesis Testing
#
# Hypothesis Testing (Section 5.1)
# Input   : data/processed/ad_clean.rds and feature_dictionary.csv
# Output  : Test results in outputs/tables/
# Execution: called automatically by the analysis.R runner.
# =============================================================================


# Load the shared data and class groups
.submission_source("R/statistical_analysis_io.R")


# -----------------------------------------------------------------------------
# 1. Test A - Do ads and nonads have different means?
# -----------------------------------------------------------------------------
# Run a two-sided Welch test for height, width and aspect ratio
# Each test starts from the hypothesis that the two class means are equal
test_a <- do.call(rbind, lapply(geometry, function(feature) {
  x <- na.omit(groups$ad[[feature]])
  y <- na.omit(groups$nonad[[feature]])
  result <- t.test(x, y)
  data.frame(feature, n_ad = length(x), n_nonad = length(y),
             difference = mean(x) - mean(y),
             ci_low = result$conf.int[1], ci_high = result$conf.int[2],
             t = unname(result$statistic), df = unname(result$parameter),
             p = result$p.value)
}))
# Adjust the three p-values with Holm to control the chance of any false positive
test_a$p_holm <- p.adjust(test_a$p, method = "holm")
write_analysis_csv(test_a, "test_a_geometry")


# -----------------------------------------------------------------------------
# 2. Test B - Is each keyword linked to the ad label?
# -----------------------------------------------------------------------------
# For each keyword, test whether its presence is independent of the ad label
test_b <- do.call(rbind, lapply(keywords, function(feature) {
    # Count absent and present keywords for nonads and ads. table() skips missing values
  tab <- table(factor(ads[[feature]], levels = 0:1), factor(ads$ad, levels = 0:1))
  row <- data.frame(feature, absent_nonad = tab[1, 1], absent_ad = tab[1, 2],
                    present_nonad = tab[2, 1], present_ad = tab[2, 2],
                    method = "Constant margin", min_expected = NA_real_,
                    chi_square = NA_real_, odds_ratio = NA_real_, p = NA_real_)

  # Skip the test if a whole row or column is empty. Keep the counts and set p to NA
  if (any(rowSums(tab) == 0) || any(colSums(tab) == 0)) return(row)
  expected <- outer(rowSums(tab), colSums(tab)) / sum(tab)
  row$min_expected <- min(expected)

  # An odds ratio above 1 means higher ad odds when the keyword is present
  row$odds_ratio <- (tab[1, 1] * tab[2, 2]) / (tab[1, 2] * tab[2, 1])

  # Use Fisher's exact test if any expected count under independence is below 5
  # Otherwise, use Pearson's chi-square test
  if (row$min_expected < 5) {
    row$method <- "Fisher"
    row$p <- fisher.test(tab)$p.value
  } else {
    result <- chisq.test(tab, correct = FALSE)
    row$method <- "Pearson"
    row$chi_square <- unname(result$statistic)
    row$p <- result$p.value
  }
  row
}))

# Use Benjamini-Hochberg to control the false discovery rate across keyword tests
test_b$p_bh <- p.adjust(test_b$p, method = "BH")
write_analysis_csv(test_b, "test_b_keywords")


# -----------------------------------------------------------------------------
# 3. Test C - Does the ad share differ by image orientation?
# -----------------------------------------------------------------------------
# Use images with a recorded aspect ratio. A ratio above 1 means a landscape image
# The null hypothesis is that landscape and other images have the same ad share
known <- ads[!is.na(ads$aratio), ]
shape <- factor(ifelse(known$aratio > 1, "landscape", "non-landscape"),
                levels = c("landscape", "non-landscape"))
total <- as.integer(table(shape))
positive <- as.integer(table(shape[known$ad == 1]))

# prop.test() without continuity correction gives the two-sided z-test p-value
# The difference and z statistic are positive when landscape images have more ads proportionally
result <- prop.test(positive, total, correct = FALSE)
pooled <- sum(positive) / sum(total)
difference <- diff(rev(positive / total))
test_c <- data.frame(n_landscape = total[1], n_non_landscape = total[2],
                     ads_landscape = positive[1], ads_non_landscape = positive[2],
                     share_landscape = positive[1] / total[1],
                     share_non_landscape = positive[2] / total[2],
                     difference, ci_low = result$conf.int[1], ci_high = result$conf.int[2],
                     z = difference / sqrt(pooled * (1 - pooled) * sum(1 / total)),
                     min_expected = min(outer(total, c(pooled, 1 - pooled))),
                     p = result$p.value)
write_analysis_csv(test_c, "test_c_proportions")
})
# END EMBEDDED SOURCE: R/06_hypothesis_testing.R

# BEGIN EMBEDDED SOURCE: R/07_logistic_regression.R
.submission_modules[["R/07_logistic_regression.R"]] <- quote({
# =============================================================================
# 07_logistic_regression.R -- Model 1: Regularized Logistic Regression
# Author  : Dinh Nguyen Viet Thang (P3)
# Project : Statistical Analysis and Comparative Evaluation of Internet
#           Advertisement Prediction Models (MT2013)
#
# Logistic Regression Model and Evaluation
# Input   : data/processed/train_raw.rds, test_raw.rds, cv_folds.rds
# Output  : outputs/predictions/logistic_predictions.csv
#           outputs/predictions/logistic_oof_predictions.csv
#           outputs/models/logistic_model.rds
#           outputs/models/logistic_params.rds
#           outputs/tables/logistic_evaluation_metrics.csv
#           outputs/tables/logistic_metrics.csv
#           outputs/tables/logistic_top_coefficients.csv
#           outputs/figures/fig_logistic_roc_pr_curve.png / .pdf
#           outputs/figures/fig_logistic_top_coefficients.png / .pdf
# =============================================================================

.submission_source("R/utils.R")
.submission_source("R/preprocessing_pipeline.R")

check_project_root()
set_project_seed()

if (!requireNamespace("glmnet", quietly = TRUE)) {
  stop("Package 'glmnet' is required to run R/07_logistic_regression.R (L2-regularized Logistic Regression). Please install it with: install.packages('glmnet')")
}

# -----------------------------------------------------------------------------
# 1. Load Shared Data & Setup Presets
# -----------------------------------------------------------------------------
message("--> [P3] Loading shared preprocessed train/test data...")
shared <- load_shared_data()
train  <- shared$train
test   <- shared$test
folds  <- shared$folds

opts   <- prep_options("logistic")
dict   <- read.csv(file.path(PATHS$processed, "feature_dictionary.csv"), stringsAsFactors = FALSE)
col_to_orig <- setNames(dict$original_name, dict$column)
col_to_orig["geom_missing"] <- "geom_missing"
col_to_orig["height"]       <- "height (log, standardized)"
col_to_orig["width"]        <- "width (log, standardized)"

# -----------------------------------------------------------------------------
# 2. Stratified 5-Fold Cross-Validation & Threshold Tuning
# -----------------------------------------------------------------------------
message("--> [P3] Running 5-fold cross-validation with L2 / Elastic-Net regularization...")

# AUC calculation function
calc_auc <- function(y, p) {
  r <- rank(p)
  n1 <- sum(y == 1)
  n0 <- sum(y == 0)
  if (n1 == 0 || n0 == 0) return(0.5)
  (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

# Precision-Recall AUC (trapezoidal rule)
calc_pr_auc <- function(y, p) {
  ord <- order(p, decreasing = TRUE)
  y_ord <- y[ord]
  tp <- cumsum(y_ord == 1)
  fp <- cumsum(y_ord == 0)
  rec <- tp / sum(y_ord == 1)
  prec <- tp / (tp + fp)
  rec <- c(0, rec)
  prec <- c(1, prec)
  sum(diff(rec) * (prec[-1] + prec[-length(prec)]) / 2)
}

# We tune lambda / C across folds
lambdas <- c(10, 2, 1, 0.5, 0.2, 0.1, 0.05, 0.01)
oof_pred_matrix <- matrix(0, nrow = nrow(train), ncol = length(lambdas))
fold_vec <- integer(nrow(train))

t_tune_start <- Sys.time()
for (k in seq_len(folds$k)) {
  fd <- cv_fold_data(k, train, folds, opts)
  tr_x <- as.matrix(fd$analysis[, -1])
  tr_y <- fd$analysis$y
  va_x <- as.matrix(fd$assessment[, -1])
  va_y <- fd$assessment$y
  val_idx <- folds$indexOut[[k]]
  fold_vec[val_idx] <- k

  w <- case_weights(tr_y)

  fit_cv <- glmnet::glmnet(tr_x, tr_y, family = "binomial", alpha = 0,
                           lambda = lambdas, weights = w)
  preds <- predict(fit_cv, va_x, type = "response")
  oof_pred_matrix[val_idx, ] <- preds
}
t_tune <- as.numeric(difftime(Sys.time(), t_tune_start, units = "secs"))

mean_pr_aucs  <- apply(oof_pred_matrix, 2, function(p) calc_pr_auc(train$y, p))
mean_roc_aucs <- apply(oof_pred_matrix, 2, function(p) calc_auc(train$y, p))

best_idx <- which.max(mean_pr_aucs)
best_lambda <- lambdas[best_idx]
best_oof_probs <- oof_pred_matrix[, best_idx]

message(sprintf("--> [P3] Best Regularization Lambda: %.4f (CV ROC-AUC: %.4f, PR-AUC: %.4f, Tuning Time: %.4fs)",
                best_lambda, mean_roc_aucs[best_idx], mean_pr_aucs[best_idx], t_tune))

# Threshold optimization on OOF predictions
thresh_seq <- seq(0.10, 0.90, by = 0.01)
f1_scores <- sapply(thresh_seq, function(th) {
  pred_bin <- as.integer(best_oof_probs >= th)
  tp <- sum(train$y == 1 & pred_bin == 1)
  fp <- sum(train$y == 0 & pred_bin == 1)
  fn <- sum(train$y == 1 & pred_bin == 0)
  if (tp == 0) return(0)
  2 * tp / (2 * tp + fp + fn)
})

best_thresh <- thresh_seq[which.max(f1_scores)]
message(sprintf("--> [P3] Optimal Decision Threshold: tau* = %.2f (OOF F1 = %.4f)",
                best_thresh, max(f1_scores)))

# -----------------------------------------------------------------------------
# 3. Fit Model on Full Training Set & Predict on Locked Test Set
# -----------------------------------------------------------------------------
message("--> [P3] Fitting final model on full training set (1,939 obs)...")
prep_full <- prep_fit(train, opts)
m_tr      <- prep_matrix(prep_full, train)
m_te      <- prep_matrix(prep_full, test)
w_full    <- case_weights(m_tr$y)

t_start <- Sys.time()
final_model <- glmnet::glmnet(m_tr$x, m_tr$y, family = "binomial", alpha = 0,
                              lambda = best_lambda, weights = w_full)
t_train <- as.numeric(difftime(Sys.time(), t_start, units = "secs"))

t_pred_start <- Sys.time()
test_probs <- as.numeric(predict(final_model, m_te$x, type = "response"))
t_pred <- as.numeric(difftime(Sys.time(), t_pred_start, units = "secs"))

coefs <- as.numeric(coef(final_model))[-1] # drop intercept
feat_names <- colnames(m_tr$x)

# Save trained model object and hyper-parameters
dir.create("outputs/models", recursive = TRUE, showWarnings = FALSE)
saveRDS(final_model, "outputs/models/logistic_model.rds")
saveRDS(list(best_lambda = best_lambda, best_threshold = best_thresh,
             weights = w_full, opts = opts,
             training_time_s = t_train, prediction_time_s = t_pred,
             tuning_time_s = t_tune),
        "outputs/models/logistic_params.rds")
message("--> [P3] Saved: outputs/models/logistic_model.rds & logistic_params.rds")

# Reproducibility metadata ----------------------------------------------------
# Record the training protocol, seed and input hashes. Separate checks verify
# the saved model, reconstructed preprocessing and exported predictions.
alignment_paths <- c("data/processed/train_raw.rds",
                     "data/processed/test_raw.rds",
                     "data/processed/cv_folds.rds")
stopifnot(all(file.exists(alignment_paths)))

saveRDS(
  list(
    model                               = "Logistic Regression",
    owner                               = "Dinh Nguyen Viet Thang (P3)",
    training_only_preprocessing         = TRUE,
    shared_cv_folds_used                = TRUE,
    threshold_selected_on_training_only = TRUE,
    test_used_for_tuning                = FALSE,
    threshold                           = best_thresh,
    threshold_rule                      = ">=",
    threshold_criterion                 = "first max OOF F1 over seq(0.10, 0.90, by = 0.01)",
    seed                                = PROJECT_SEED,
    cv_folds                            = folds$k,
    input_md5                           = tools::md5sum(alignment_paths),
    train_ids                           = as.integer(train$obs_id),
    test_ids                            = as.integer(test$obs_id),
    cv_fold_id                          = as.integer(folds$fold_id),
    # Precise timings are verified from logistic_params.rds and the metric CSVs.
    # Omit duplicate numeric metadata to avoid character round-trip precision
    # loss; retain explicit definitions of each measured interval below.
    timing_boundaries = paste(
      "Training_time_s and Prediction_time_s are the final model refit on the",
      "full training set (n = 1939) and prediction on the locked test set",
      "(n = 484); they are not CV fold times. Tuning_time_s is the total",
      "wall-clock time of the 5-fold CV grid search over the 8 lambda values."
    ),
    software_versions = c(R      = as.character(getRversion()),
                          glmnet = as.character(packageVersion("glmnet"))),
    run_timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  ),
  "outputs/models/logistic_run_metadata.rds"
)
message("--> [P3] Saved: outputs/models/logistic_run_metadata.rds")

# -----------------------------------------------------------------------------
# 4. Save Predictions Files (Test Predictions and OOF Predictions)
# -----------------------------------------------------------------------------
dir.create("outputs/predictions", recursive = TRUE, showWarnings = FALSE)

test_preds_binary <- as.integer(test_probs >= best_thresh)
pred_export <- data.frame(
  sample_id      = if ("sample_id" %in% names(test)) test$sample_id else test$obs_id,
  actual         = test$y,
  predicted      = test_preds_binary,
  probability_ad = round(test_probs, 6)
)
if ("obs_id" %in% names(test)) pred_export$obs_id <- test$obs_id
write.csv(pred_export, "outputs/predictions/logistic_predictions.csv", row.names = FALSE)
message("--> [P3] Saved: outputs/predictions/logistic_predictions.csv (", nrow(pred_export), " rows)")

oof_preds_binary <- as.integer(best_oof_probs >= best_thresh)
oof_export <- data.frame(
  sample_id      = if ("sample_id" %in% names(train)) train$sample_id else train$obs_id,
  actual         = train$y,
  predicted      = oof_preds_binary,
  probability_ad = round(best_oof_probs, 6),
  fold           = fold_vec
)
write.csv(oof_export, "outputs/predictions/logistic_oof_predictions.csv", row.names = FALSE)
message("--> [P3] Saved: outputs/predictions/logistic_oof_predictions.csv (", nrow(oof_export), " rows)")

# -----------------------------------------------------------------------------
# 5. Compute Metrics & Save Tables
# -----------------------------------------------------------------------------
compute_metrics <- function(y, p, th) {
  pred <- as.integer(p >= th)
  tp <- sum(y == 1 & pred == 1)
  fp <- sum(y == 0 & pred == 1)
  fn <- sum(y == 1 & pred == 0)
  tn <- sum(y == 0 & pred == 0)

  acc  <- (tp + tn) / length(y)
  prec <- if ((tp + fp) > 0) tp / (tp + fp) else 0
  rec  <- if ((tp + fn) > 0) tp / (tp + fn) else 0
  spec <- if ((tn + fp) > 0) tn / (tn + fp) else 0
  f1   <- if ((prec + rec) > 0) 2 * prec * rec / (prec + rec) else 0
  roc  <- calc_auc(y, p)
  pr   <- calc_pr_auc(y, p)

  list(Accuracy = acc, Precision = prec, Recall = rec, Specificity = spec,
       F1 = f1, ROC_AUC = roc, PR_AUC = pr, TP = tp, FP = fp, FN = fn, TN = tn)
}

m_oof <- compute_metrics(train$y, best_oof_probs, best_thresh)
m_def <- compute_metrics(test$y, test_probs, 0.50)
m_opt <- compute_metrics(test$y, test_probs, best_thresh)

oof_col_name <- sprintf("OOF Validation (tau* = %.2f)", best_thresh)
opt_col_name <- sprintf("Test Set (tau* = %.2f)", best_thresh)
metrics_tab <- data.frame(
  Metric = c("Accuracy", "Precision (ad)", "Recall (ad)", "Specificity (nonad)",
             "F1-Score (ad)", "ROC-AUC", "PR-AUC",
             "True Positives (TP)", "False Positives (FP)",
             "False Negatives (FN)", "True Negatives (TN)",
             "Tuning Time (s)", "Training Time (s)", "Prediction Time (s)"),
  oof_col = c(
    sprintf("%.4f", m_oof$Accuracy), sprintf("%.4f", m_oof$Precision),
    sprintf("%.4f", m_oof$Recall), sprintf("%.4f", m_oof$Specificity),
    sprintf("%.4f", m_oof$F1), sprintf("%.4f", m_oof$ROC_AUC),
    sprintf("%.4f", m_oof$PR_AUC), m_oof$TP, m_oof$FP, m_oof$FN, m_oof$TN,
    sprintf("%.4f s", t_tune), "---", "---"
  ),
  def_col = c(
    sprintf("%.4f", m_def$Accuracy), sprintf("%.4f", m_def$Precision),
    sprintf("%.4f", m_def$Recall), sprintf("%.4f", m_def$Specificity),
    sprintf("%.4f", m_def$F1), sprintf("%.4f", m_def$ROC_AUC),
    sprintf("%.4f", m_def$PR_AUC), m_def$TP, m_def$FP, m_def$FN, m_def$TN,
    sprintf("%.4f s", t_tune), sprintf("%.4f s", t_train), sprintf("%.4f s", t_pred)
  ),
  opt_col = c(
    sprintf("%.4f", m_opt$Accuracy), sprintf("%.4f", m_opt$Precision),
    sprintf("%.4f", m_opt$Recall), sprintf("%.4f", m_opt$Specificity),
    sprintf("%.4f", m_opt$F1), sprintf("%.4f", m_opt$ROC_AUC),
    sprintf("%.4f", m_opt$PR_AUC), m_opt$TP, m_opt$FP, m_opt$FN, m_opt$TN,
    sprintf("%.4f s", t_tune), sprintf("%.4f s", t_train), sprintf("%.4f s", t_pred)
  ),
  check.names = FALSE
)
names(metrics_tab) <- c("Metric", oof_col_name, "Test Set (tau = 0.50)", opt_col_name)

dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)
write.csv(metrics_tab, "outputs/tables/logistic_evaluation_metrics.csv", row.names = FALSE)
message("--> [P3] Saved: outputs/tables/logistic_evaluation_metrics.csv")

# Also write Shape A metrics table for robust P4 compatibility
logistic_metrics_simple <- data.frame(
  metric = c("Training_time_s", "Tuning_time_s", "Prediction_time_s",
             "Accuracy", "Precision", "Recall", "Specificity", "F1", "ROC_AUC", "PR_AUC"),
  value  = c(t_train, t_tune, t_pred,
             m_opt$Accuracy, m_opt$Precision, m_opt$Recall, m_opt$Specificity,
             m_opt$F1, m_opt$ROC_AUC, m_opt$PR_AUC)
)
write.csv(logistic_metrics_simple, "outputs/tables/logistic_metrics.csv", row.names = FALSE)
message("--> [P3] Saved: outputs/tables/logistic_metrics.csv")

# Save Top Coefficients
orig_names <- unname(sapply(feat_names, function(c) if (!is.na(col_to_orig[c])) col_to_orig[c] else c))
coef_df <- data.frame(
  feature_col   = feat_names,
  original_name = orig_names,
  coefficient   = coefs,
  odds_ratio    = exp(coefs),
  abs_coef      = abs(coefs)
)
coef_df <- coef_df[order(-coef_df$abs_coef), ]
write.csv(coef_df, "outputs/tables/logistic_top_coefficients.csv", row.names = FALSE)
message("--> [P3] Saved: outputs/tables/logistic_top_coefficients.csv")

# -----------------------------------------------------------------------------
# 6. Generate & Save Figures (ROC/PR Curves & Top 20 Coefficients)
# -----------------------------------------------------------------------------
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("report/Part_3/figures", recursive = TRUE, showWarnings = FALSE)

plot_roc_pr <- function(y, p, roc_auc, pr_auc) {
  ord <- order(p, decreasing = TRUE)
  y_ord <- y[ord]
  tp <- cumsum(y_ord == 1)
  fp <- cumsum(y_ord == 0)
  tpr <- c(0, tp / sum(y_ord == 1))
  fpr <- c(0, fp / sum(y_ord == 0))
  rec <- c(0, tp / sum(y_ord == 1))
  prec <- c(1, tp / (tp + fp))
  baseline <- mean(y == 1)

  op <- par(mfrow = c(1, 2), mar = c(4.5, 4.5, 2.8, 1.2), mgp = c(2.6, 0.8, 0))
  on.exit(par(op), add = TRUE)

  # Panel 1: ROC
  plot(fpr, tpr, type = "n", xlim = c(0, 1), ylim = c(0, 1.02),
       xlab = "False Positive Rate (1 - Specificity)",
       ylab = "True Positive Rate (Recall)",
       main = "Receiver Operating Characteristic (ROC)",
       cex.main = 1.1, cex.lab = 1.0, cex.axis = 0.9, bty = "o")
  grid(col = "#E5E7EB", lty = 1)
  abline(0, 1, col = "#777777", lty = 2, lwd = 1.2)
  lines(fpr, tpr, col = "#E8632B", lwd = 2.4)
  legend("bottomright",
         legend = sprintf("Test ROC (AUC = %.4f)", roc_auc),
         col = "#E8632B", lwd = 2.4, bg = "white", box.col = "#CCCCCC", cex = 0.9)

  # Panel 2: PR
  plot(rec, prec, type = "n", xlim = c(0, 1), ylim = c(0.12, 1.03),
       xlab = "Recall", ylab = "Precision",
       main = "Precision-Recall (PR) Curve",
       cex.main = 1.1, cex.lab = 1.0, cex.axis = 0.9, bty = "o")
  grid(col = "#E5E7EB", lty = 1)
  abline(h = baseline, col = "#777777", lty = 2, lwd = 1.2)
  lines(rec, prec, col = "#2171D6", lwd = 2.4)
  legend("bottomleft",
         legend = c(sprintf("Test PR (AUC = %.4f)", pr_auc),
                    sprintf("Baseline (%.3f)", baseline)),
         col = c("#2171D6", "#777777"), lwd = c(2.4, 1.2), lty = c(1, 2),
         bg = "white", box.col = "#CCCCCC", cex = 0.9)
}

plot_top_coefs <- function(coef_df) {
  top20 <- head(coef_df, 20)
  top20 <- top20[order(top20$coefficient), ]
  cols <- ifelse(top20$coefficient >= 0, "#E8632B", "#2171D6")
  xlim_pad <- max(abs(top20$coefficient)) * 1.12

  op <- par(mar = c(4.5, 14.5, 2.8, 1.5), mgp = c(2.6, 0.7, 0))
  on.exit(par(op), add = TRUE)

  bp <- barplot(top20$coefficient, horiz = TRUE, col = cols, border = NA,
                names.arg = top20$original_name, las = 1,
                xlim = c(-xlim_pad, xlim_pad),
                xlab = "Logistic Regression Coefficient (log-odds)",
                main = "Top 20 Predictors by Magnitude in Regularized Logistic Regression",
                cex.names = 0.85, cex.main = 1.05, cex.lab = 1.0, cex.axis = 0.9)
  abline(v = pretty(c(-xlim_pad, xlim_pad)), col = "#E5E7EB", lty = 1)
  barplot(top20$coefficient, horiz = TRUE, col = cols, border = NA,
          names.arg = rep("", nrow(top20)), add = TRUE, axes = FALSE)
  abline(v = 0, col = "#222222", lwd = 1.1)
  box()
}

for (fig_dir in c("outputs/figures", "report/Part_3/figures")) {
  png(file.path(fig_dir, "fig_logistic_roc_pr_curve.png"), width = 10, height = 4.6, units = "in", res = 300)
  plot_roc_pr(test$y, test_probs, m_opt$ROC_AUC, m_opt$PR_AUC)
  dev.off()

  pdf(file.path(fig_dir, "fig_logistic_roc_pr_curve.pdf"), width = 10, height = 4.6)
  plot_roc_pr(test$y, test_probs, m_opt$ROC_AUC, m_opt$PR_AUC)
  dev.off()

  png(file.path(fig_dir, "fig_logistic_top_coefficients.png"), width = 8.5, height = 6.2, units = "in", res = 300)
  plot_top_coefs(coef_df)
  dev.off()

  pdf(file.path(fig_dir, "fig_logistic_top_coefficients.pdf"), width = 8.5, height = 6.2)
  plot_top_coefs(coef_df)
  dev.off()
}
message("--> [P3] Saved updated figures to outputs/figures/ and report/Part_3/figures/")

message("--> [P3] Model 1 - Logistic Regression pipeline finished successfully!")
})
# END EMBEDDED SOURCE: R/07_logistic_regression.R

# BEGIN EMBEDDED SOURCE: R/08_random_forest.R
.submission_modules[["R/08_random_forest.R"]] <- quote({
# =============================================================================
# P4 / Quynh Mai: Random Forest tuning, final evaluation and interpretation.
# Execution: called automatically after reconstructing the shared data from CSV.
# Requires randomForest; uses the fixed train/test split and five CV folds.
# =============================================================================

.submission_source("R/p4_helpers.R")
.submission_source("R/p4_rf_functions.R")
shared <- p4_init()
if (!requireNamespace("randomForest", quietly = TRUE)) {
  stop("Install package 'randomForest' before running this script.", call. = FALSE)
}
train <- shared$train
test <- shared$test
folds <- shared$folds
seed <- 42L
opts <- prep_options("random_forest")
stopifnot(!anyDuplicated(train$obs_id), !anyDuplicated(test$obs_id),
          !any(train$obs_id %in% test$obs_id))

# Cache transformed folds, never a preprocessing fit on all train before CV.
# Reusing these fixed folds across configurations does not leak validation data.
tuning_start <- proc.time()[["elapsed"]]
fold_data <- lapply(seq_len(folds$k), function(k) {
  cv_fold_data(k, train, folds, opts, target = "factor")
})
reference_p <- ncol(fold_data[[1L]]$analysis) - 1L
mtry_values <- unique(pmax(1L, as.integer(c(floor(sqrt(reference_p) / 2),
                                        floor(sqrt(reference_p)),
                                        floor(2 * sqrt(reference_p))))))
ntree_values <- c(200L, 400L)
grid <- expand.grid(ntree = ntree_values, mtry = mtry_values,
                    nodesize = c(1L, 5L), KEEP.OUT.ATTRS = FALSE)
grid$config_id <- seq_len(nrow(grid))
fold_records <- list()
candidate_oof <- lapply(seq_len(nrow(grid)), function(g) {
  list(probability = rep(NA_real_, nrow(train)),
       fold = rep(NA_integer_, nrow(train)))
})
summary_records <- vector("list", nrow(grid))
fit_groups <- unique(grid[, c("mtry", "nodesize"), drop = FALSE])
workers <- p4_rf_worker_count(folds$k)
cat(sprintf("RF cross-validation uses %d worker(s).\n", workers))
# Persistent workers source project functions once. on.exit runs even if a fold
# fails, so completed or interrupted runs do not leave a socket cluster behind.
cv_run <- (function() {
  cluster <- if (workers > 1L) p4_rf_make_cluster(workers) else NULL
  if (!is.null(cluster)) on.exit(parallel::stopCluster(cluster), add = TRUE)
  tasks <- lapply(seq_len(folds$k), function(k) {
    fd <- fold_data[[k]]
    list(k = k, analysis = fd$analysis, assessment = fd$assessment,
         assessment_obs_id = fd$assessment_obs_id)
  })

  for (j in seq_len(nrow(fit_groups))) {
    config <- list(ntree = max(ntree_values), mtry = fit_groups$mtry[[j]],
                   nodesize = fit_groups$nodesize[[j]])
    candidates <- which(grid$mtry == config$mtry & grid$nodesize == config$nodesize)
    cat(sprintf("RF shared forest %d/%d: trees=%d, mtry=%d, nodesize=%d (prefixes %s)\n",
                j, nrow(fit_groups), config$ntree, config$mtry, config$nodesize,
                paste(ntree_values, collapse = "/")))
    results <- if (is.null(cluster)) {
      lapply(tasks, p4_rf_cv_task, config = config, seed = seed,
               ntree_values = ntree_values)
    } else {
      parallel::parLapplyLB(cluster, tasks, p4_rf_cv_task, config = config,
                            seed = seed, ntree_values = ntree_values)
    }
    for (result in results) {
      k <- result$k
      rows <- match(result$assessment_obs_id, train$obs_id)
      stopifnot(!anyNA(rows), !anyDuplicated(rows))
      for (g in candidates) {
        probability <- result$probabilities[[as.character(grid$ntree[[g]])]]
        stopifnot(all(is.na(candidate_oof[[g]]$probability[rows])))
        candidate_oof[[g]]$probability[rows] <- probability
        candidate_oof[[g]]$fold[rows] <- k
        fold_metrics <- p4_metrics(result$actual, probability)
        fold_records[[length(fold_records) + 1L]] <- data.frame(
          config_id = g, shared_fit_group = j, fold = k,
          ntree = grid$ntree[[g]], fitted_ntree = config$ntree, mtry = config$mtry,
          effective_mtry = result$effective_mtry, nodesize = config$nodesize,
          n_predictors = result$n_predictors, seed = result$seed,
          PR_AUC = unname(fold_metrics[["PR_AUC"]]),
          ROC_AUC = unname(fold_metrics[["ROC_AUC"]]),
          F1_at_0_5 = unname(fold_metrics[["F1"]]),
          shared_max_tree_fit_seconds = result$shared_max_tree_fit_seconds,
          shared_prefix_prediction_seconds = result$shared_prefix_prediction_seconds
        )
      }
    }
  }
  list(fold_records = fold_records, candidate_oof = candidate_oof)
})()
fold_records <- cv_run$fold_records
candidate_oof <- cv_run$candidate_oof
rm(cv_run)

fold_results <- do.call(rbind, fold_records)
for (g in seq_len(nrow(grid))) {
  oof <- candidate_oof[[g]]$probability
  oof_fold <- candidate_oof[[g]]$fold
  stopifnot(!anyNA(oof), !anyNA(oof_fold), all(is.finite(oof)))
  current <- fold_results[fold_results$config_id == g, , drop = FALSE]
  oof_metrics <- p4_metrics(train$y, oof)
  summary_records[[g]] <- data.frame(
    config_id = g, ntree = grid$ntree[[g]], mtry = grid$mtry[[g]],
    nodesize = grid$nodesize[[g]],
    mean_PR_AUC = mean(current$PR_AUC), sd_PR_AUC = stats::sd(current$PR_AUC),
    mean_ROC_AUC = mean(current$ROC_AUC),
    OOF_F1_at_0_5 = unname(oof_metrics[["F1"]]),
    shared_max_tree_fit_seconds = sum(current$shared_max_tree_fit_seconds),
    shared_prefix_prediction_seconds = sum(current$shared_prefix_prediction_seconds)
  )
}

cv_results <- do.call(rbind, summary_records)
# Primary ranking is mean fold PR-AUC. Only exact ties use OOF F1 at 0.5,
# then the smaller forest / mtry / node size. Test labels never enter selection.
ranked <- order(-cv_results$mean_PR_AUC, -cv_results$OOF_F1_at_0_5,
                cv_results$ntree, cv_results$mtry, cv_results$nodesize)
selected <- cv_results[ranked[[1L]], , drop = FALSE]
selected_id <- selected$config_id[[1L]]
oof_probability <- candidate_oof[[selected_id]]$probability
oof_fold <- candidate_oof[[selected_id]]$fold
threshold_choice <- p4_choose_threshold(train$y, oof_probability)
threshold <- threshold_choice$threshold
tuning_seconds <- proc.time()[["elapsed"]] - tuning_start
cv_results$selected <- cv_results$config_id == selected_id
p4_write_table(cv_results, "random_forest_cv_grid",
               "Random Forest shared-fold cross-validation results.",
               "Selection maximizes mean fold PR-AUC; F1 at 0.5 breaks exact ties. Each 400-tree forest supplies 200/400-tree prefix votes. Shared fit/prediction times repeat across the paired candidates and must not be summed across them. Preprocessing is fitted within each analysis fold.")
p4_write_manifest(fold_results, "random_forest_cv_fold_metrics")
p4_write_manifest(threshold_choice$table, "random_forest_oof_threshold_search")

params <- list(ntree = as.integer(selected$ntree[[1L]]),
               mtry = as.integer(selected$mtry[[1L]]),
               nodesize = as.integer(selected$nodesize[[1L]]),
               threshold = threshold, seed = seed, opts = opts,
               cv_folds = folds$k, cv_seed_rule = "seed + fold - 1",
               selection_metric = "mean fold PR_AUC", reference_p = reference_p,
               tuning_method = "200/400-tree prefixes from shared 400-tree forests",
               mtry_grid = mtry_values, cv_workers = workers,
               tuning_seconds = tuning_seconds)

# Fit preprocessing on the full training partition only after CV is complete.
prep <- prep_fit(train, opts)
tr <- prep_apply(prep, train, target = "factor")
te <- prep_apply(prep, test, target = "factor")
fit_start <- proc.time()[["elapsed"]]
model <- p4_rf_fit(tr, params, seed, importance = TRUE)
fit_seconds <- proc.time()[["elapsed"]] - fit_start
predict_start <- proc.time()[["elapsed"]]
test_probability <- p4_rf_probability(model, te)
prediction_seconds <- proc.time()[["elapsed"]] - predict_start
train_probability <- p4_rf_probability(model, tr)
test_predicted <- as.integer(test_probability >= threshold)
oof_predicted <- as.integer(oof_probability >= threshold)

prediction <- data.frame(sample_id = test$obs_id, actual = test$y,
                         predicted = test_predicted, probability_ad = test_probability)
oof_prediction <- data.frame(sample_id = train$obs_id, actual = train$y,
                             predicted = oof_predicted, probability_ad = oof_probability,
                             fold = oof_fold)
write.csv(prediction, "outputs/predictions/random_forest_predictions.csv", row.names = FALSE)
write.csv(oof_prediction, "outputs/predictions/random_forest_oof_predictions.csv", row.names = FALSE)
params$effective_mtry <- model$p4_effective_mtry
params$n_predictors <- length(model$p4_predictors)
params$training_seconds <- fit_seconds
params$prediction_seconds <- prediction_seconds
saveRDS(list(model = model, prep = prep, params = params),
        "outputs/models/random_forest_model.rds")
saveRDS(params, "outputs/models/random_forest_params.rds")

metric_sets <- list(
  Train = p4_metrics(train$y, train_probability, threshold = threshold),
  OOF = p4_metrics(train$y, oof_probability, threshold = threshold),
  Test = p4_metrics(test$y, test_probability, threshold = threshold)
)
metric_table <- do.call(rbind, lapply(names(metric_sets), function(dataset) {
  values <- metric_sets[[dataset]]
  data.frame(model = "Random Forest", dataset = dataset,
             metric = names(values), value = unname(values))
}))
metric_table <- rbind(metric_table, data.frame(
  model = "Random Forest", dataset = "Test",
  metric = c("Training_time_s", "Prediction_time_s", "Tuning_time_s"),
  value = c(fit_seconds, prediction_seconds, tuning_seconds)
))
p4_write_manifest(metric_table, "random_forest_metrics")
performance <- data.frame(
  Metric = c("Accuracy", "Precision", "Recall", "Specificity", "F1", "ROC_AUC", "PR_AUC"),
  stringsAsFactors = FALSE
)
for (dataset in names(metric_sets)) performance[[dataset]] <- metric_sets[[dataset]][performance$Metric]
p4_write_table(performance, "random_forest_performance",
               "Random Forest training, out-of-fold and test performance.",
               "The threshold is selected from OOF predictions. Training metrics are descriptive; OOF performance also informed configuration selection and is not an independent test estimate.")
parameter_table <- data.frame(
  Parameter = c("Trees", "mtry", "Minimum node size", "Classification threshold",
                "Seed", "CV folds", "CV workers", "Predictors", "CV/tuning seconds",
                "Final fitting seconds", "Test prediction seconds"),
  Value = c(params$ntree, params$effective_mtry, params$nodesize, threshold,
            seed, folds$k, workers, params$n_predictors, tuning_seconds, fit_seconds,
            prediction_seconds)
)
p4_write_table(parameter_table, "random_forest_parameters",
               "Selected Random Forest configuration and computational cost.",
               "Class weights are computed from each training partition. Fitting time excludes tuning and preprocessing; the final fit includes permutation importance.")

test_metrics <- metric_sets$Test
confusion <- data.frame(
  Actual = c("ad", "nonad"),
  Predicted_ad = c(test_metrics[["TP"]], test_metrics[["FP"]]),
  Predicted_nonad = c(test_metrics[["FN"]], test_metrics[["TN"]])
)
p4_write_table(confusion, "random_forest_confusion_matrix",
               "Random Forest confusion matrix on the locked test set.")

# Permutation importance is computed on out-of-bag observations within the
# training forest. Test labels are not used to rank features.
imp <- randomForest::importance(model, type = 1, scale = FALSE)
if (is.matrix(imp)) {
  imp_column <- if ("MeanDecreaseAccuracy" %in% colnames(imp)) "MeanDecreaseAccuracy" else colnames(imp)[ncol(imp)]
  importance_values <- imp[, imp_column]
} else {
  importance_values <- imp
}
importance_table <- data.frame(column = names(importance_values),
                                permutation_importance = as.numeric(importance_values),
                                stringsAsFactors = FALSE)
dictionary <- read.csv("data/processed/feature_dictionary.csv", stringsAsFactors = FALSE)
importance_table$feature <- dictionary$original_name[match(importance_table$column, dictionary$column)]
importance_table$feature[is.na(importance_table$feature)] <- importance_table$column[is.na(importance_table$feature)]
importance_table$feature[importance_table$column == "geom_missing"] <- "Any geometry value missing"
importance_table <- importance_table[order(-importance_table$permutation_importance,
                                          importance_table$column), ]
rownames(importance_table) <- NULL
p4_write_manifest(importance_table, "random_forest_feature_importance")
top <- head(importance_table, 15L)
p4_write_table(data.frame(Rank = seq_len(nrow(top)), Feature = top$feature,
                         Permutation_importance = top$permutation_importance),
               "random_forest_top_features", "Top Random Forest predictors by permutation importance.",
               "Unscaled mean decrease in out-of-bag accuracy. Importance indicates predictive contribution, not causal direction; correlated features can share importance.")
p4_plot("random_forest_feature_importance", function() {
  par(mar = c(4.5, 14, 2, 1))
  barplot(rev(top$permutation_importance),
          names.arg = rev(substr(top$feature, 1, 48)), horiz = TRUE,
          las = 1, cex.names = 0.75, col = "#2a78d6", border = NA,
          xlab = "Mean decrease in out-of-bag accuracy")
}, width = 8, height = 5.5)

wrong <- prediction$actual != prediction$predicted
errors <- prediction[wrong, , drop = FALSE]
errors$error_type <- ifelse(errors$actual == 1L, "False negative", "False positive")
errors$geom_missing <- as.integer(rowSums(is.na(test[wrong, NUMERIC_VARS, drop = FALSE])) > 0)
errors$local_missing <- as.integer(is.na(test$local[wrong]))
errors$local <- test$local[wrong]
for (v in NUMERIC_VARS) errors[[v]] <- test[[v]][wrong]
p4_write_manifest(errors, "random_forest_prediction_errors")
p4_write_manifest(data.frame(
  artifact = c("prediction", "oof_prediction", "model", "params"),
  path = c("outputs/predictions/random_forest_predictions.csv",
           "outputs/predictions/random_forest_oof_predictions.csv",
           "outputs/models/random_forest_model.rds",
           "outputs/models/random_forest_params.rds"),
  seed = seed
), "random_forest_artifacts")
cat(sprintf("RF finished: threshold=%.2f; test PR-AUC=%.4f; test F1=%.4f\n",
            threshold, test_metrics[["PR_AUC"]], test_metrics[["F1"]]))
})
# END EMBEDDED SOURCE: R/08_random_forest.R

# BEGIN EMBEDDED SOURCE: R/09_xgboost.R
.submission_modules[["R/09_xgboost.R"]] <- quote({
# ==============================================================================
# R/09_xgboost.R -- XGBoost Classifier (Leakage-Safe Shared 5-Fold CV)
# MT2013 Probability & Statistics | Team 12 | Author: Thanh Tai (P5)
# ==============================================================================

.submission_source("R/utils.R")
.submission_source("R/preprocessing_pipeline.R")

check_project_root()
stopifnot(requireNamespace("xgboost", quietly = TRUE))
library(ggplot2)

# Create output directories if missing
dir.create("outputs/models", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/predictions", recursive = TRUE, showWarnings = FALSE)

set_project_seed()

# 1. Load & validate shared data -----------------------------------------------
shared <- load_shared_data()
train  <- shared$train
test   <- shared$test
folds  <- shared$folds

opts <- prep_options("xgboost")

stopifnot(
  folds$k == 5L,
  all(train$y %in% c(0, 1)),
  all(test$y %in% c(0, 1)),
  !anyDuplicated(train$obs_id),
  !anyDuplicated(test$obs_id),
  length(intersect(train$obs_id, test$obs_id)) == 0L,
  all(sort(unlist(folds$indexOut)) == seq_len(nrow(train)))
)

cat(sprintf("Loaded shared data: train = %d, test = %d, folds = %d\n",
            nrow(train), nrow(test), folds$k))

# 2. Evaluation metric functions -----------------------------------------------
roc_auc <- function(y, p) {
  stopifnot(length(y) == length(p), all(is.finite(p)))
  n1 <- sum(y == 1); n0 <- sum(y == 0)
  if (n1 == 0 || n0 == 0) stop("ROC-AUC: only one class present")
  (sum(rank(p, ties.method = "average")[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

# Average Precision (ranked precision at positive observations)
avg_precision <- function(y, p) {
  stopifnot(length(y) == length(p), all(is.finite(p)))
  if (sum(y == 1) == 0) stop("AP: no positive class")
  ord <- order(-p, seq_along(p))
  yy  <- y[ord]
  mean((cumsum(yy == 1) / seq_along(yy))[yy == 1])
}

# Native XGBoost PR-AUC uses the PRROC integral; a trapezoidal fallback is defined.
# The runner requires PRROC. Paired comparisons use a separate tie-grouped metric.
pr_auc_trapz <- function(y, p) {
  stopifnot(length(y) == length(p), all(is.finite(p)))
  if (requireNamespace("PRROC", quietly = TRUE)) {
    pr <- PRROC::pr.curve(scores.class0 = p[y == 1],
                          scores.class1 = p[y == 0], curve = FALSE)
    return(as.numeric(pr$auc.integral))
  }
  n1  <- sum(y == 1)
  ord <- order(p, decreasing = TRUE)
  yy  <- y[ord]
  tp  <- cumsum(yy == 1); fp <- cumsum(yy == 0)
  rec <- c(0, tp / n1); prec <- c(1, tp / (tp + fp))
  sum(diff(rec) * (prec[-1] + prec[-length(prec)]) / 2)
}

score_metrics <- function(y, p, threshold) {
  pred <- as.integer(p >= threshold)
  tp <- sum(y == 1 & pred == 1); fp <- sum(y == 0 & pred == 1)
  fn <- sum(y == 1 & pred == 0); tn <- sum(y == 0 & pred == 0)

  precision   <- if (tp + fp == 0) 0 else tp / (tp + fp)
  recall      <- if (tp + fn == 0) 0 else tp / (tp + fn)
  specificity <- if (tn + fp == 0) 0 else tn / (tn + fp)
  f1          <- if (2 * tp + fp + fn == 0) 0 else 2 * tp / (2 * tp + fp + fn)

  data.frame(
    Threshold         = threshold,
    Accuracy          = (tp + tn) / length(y),
    Precision         = precision,
    Recall            = recall,
    Specificity       = specificity,
    F1_Score          = f1,
    ROC_AUC           = roc_auc(y, p),
    PR_AUC            = pr_auc_trapz(y, p),
    Average_Precision = avg_precision(y, p),
    TP = tp, FP = fp, FN = fn, TN = tn,
    stringsAsFactors  = FALSE
  )
}

make_dmat <- function(df, with_label = TRUE) {
  x <- as.matrix(df[, setdiff(names(df), "y"), drop = FALSE])
  if (with_label) {
    xgboost::xgb.DMatrix(x, label = df$y, missing = NA)
  } else {
    xgboost::xgb.DMatrix(x, missing = NA)
  }
}

# 3. Hyperparameter grid with Regularization (5-Fold CV) -----------------------
grid <- data.frame(
  eta              = c(0.10, 0.05, 0.05, 0.05, 0.03, 0.03),
  max_depth        = c(3L,   4L,   5L,   6L,   4L,   5L),
  nrounds          = c(150L, 250L, 250L, 250L, 350L, 350L),
  min_child_weight = c(1,    1,    3,    3,    1,    3),
  subsample        = rep(0.8, 6),
  colsample_bytree = rep(0.7, 6),
  lambda           = c(1.0,  1.0,  1.5,  2.0,  1.0,  2.0),
  alpha            = c(0.0,  0.5,  0.5,  1.0,  0.5,  1.0),
  stringsAsFactors = FALSE
)

cv_rows <- vector("list", nrow(grid))
all_oof <- vector("list", nrow(grid))

cat("\n--- Starting 5-Fold Cross-Validation Tuning ---\n")
# Tuning cost is reported separately from the final fit: Section 5.5.3
# (Computational Efficiency) compares total search cost across models, and the
# final-model training time alone understates XGBoost by two orders of magnitude.
t_tune_start <- proc.time()
for (i in seq_len(nrow(grid))) {
  oof      <- rep(NA_real_, nrow(train))
  fold_ap  <- numeric(folds$k)
  fold_pr  <- numeric(folds$k)
  fold_roc <- numeric(folds$k)

  for (k in seq_len(folds$k)) {
    # Fit preprocessing strictly inside each analysis fold
    fd  <- cv_fold_data(k, train, folds, opts)
    dtr <- make_dmat(fd$analysis, with_label = TRUE)
    dva <- make_dmat(fd$assessment, with_label = FALSE)

    params <- list(
      objective        = "binary:logistic",
      eval_metric      = "aucpr",
      eta              = grid$eta[i],
      max_depth        = grid$max_depth[i],
      min_child_weight = grid$min_child_weight[i],
      subsample        = grid$subsample[i],
      colsample_bytree = grid$colsample_bytree[i],
      lambda           = grid$lambda[i],
      alpha            = grid$alpha[i],
      scale_pos_weight = scale_pos_weight(fd$analysis$y),
      nthread          = 1,
      seed             = PROJECT_SEED
    )

    set_project_seed(PROJECT_SEED + i * 10L + k)
    model <- xgboost::xgb.train(
      params  = params,
      data    = dtr,
      nrounds = grid$nrounds[i],
      verbose = 0
    )

    pred <- as.numeric(predict(model, dva))
    ix   <- folds$indexOut[[k]]
    stopifnot(length(pred) == length(ix), all(is.finite(pred)))

    oof[ix]     <- pred
    fold_ap[k]  <- avg_precision(fd$assessment$y, pred)
    fold_pr[k]  <- pr_auc_trapz(fd$assessment$y, pred)
    fold_roc[k] <- roc_auc(fd$assessment$y, pred)
  }

  stopifnot(!anyNA(oof))
  all_oof[[i]] <- oof

  cv_rows[[i]] <- cbind(
    config             = i,
    grid[i, ],
    mean_fold_PR_AUC   = mean(fold_pr),
    sd_fold_PR_AUC     = sd(fold_pr),
    mean_fold_AP       = mean(fold_ap),
    mean_fold_ROC_AUC  = mean(fold_roc),
    pooled_OOF_PR_AUC  = pr_auc_trapz(train$y, oof),
    pooled_OOF_ROC_AUC = roc_auc(train$y, oof)
  )

  cat(sprintf("Config %d/%d (eta=%.2f, depth=%d, rounds=%d) -> Mean PR-AUC: %.4f | Mean ROC-AUC: %.4f\n",
              i, nrow(grid), grid$eta[i], grid$max_depth[i], grid$nrounds[i],
              mean(fold_pr), mean(fold_roc)))
}

tuning_time_sec <- as.numeric((proc.time() - t_tune_start)["elapsed"])
cat(sprintf("Total 5-fold CV tuning time: %.2f sec (%d configs)\n",
            tuning_time_sec, nrow(grid)))

cv_tab <- do.call(rbind, cv_rows)
write.csv(cv_tab, "outputs/tables/xgboost_cv_grid.csv", row.names = FALSE)

# 4. Select best configuration & tune classification threshold on OOF ----------
best_i <- which.max(cv_tab$mean_fold_PR_AUC)
best   <- grid[best_i, ]
oof    <- all_oof[[best_i]]

thresholds <- seq(0.10, 0.90, by = 0.01)
thresh_df  <- do.call(rbind, lapply(thresholds, function(t) score_metrics(train$y, oof, t)))
write.csv(thresh_df, "outputs/tables/xgboost_threshold_tuning.csv", row.names = FALSE)

best_threshold <- thresh_df$Threshold[which.max(thresh_df$F1_Score)]
cat(sprintf("\nSelected Config: %d | Optimal OOF Threshold (max F1): %.2f (OOF F1 = %.4f)\n",
            best_i, best_threshold, max(thresh_df$F1_Score)))

# Export sample IDs, labels, probabilities and shared fold IDs so comparison
# checks can verify alignment with the fixed five-fold split.
write.csv(
  data.frame(
    sample_id      = train$obs_id,
    actual         = train$y,
    predicted      = as.integer(oof >= best_threshold),
    probability_ad = oof,
    fold           = as.integer(folds$fold_id)
  ),
  "outputs/predictions/xgboost_oof_predictions.csv",
  row.names = FALSE
)

# 5. Final fit on full training set & evaluate on LOCKED test set --------------
prep_final <- prep_fit(train, opts)
tr <- prep_matrix(prep_final, train)
te <- prep_matrix(prep_final, test)
stopifnot(identical(colnames(tr$x), colnames(te$x)))

dtr <- xgboost::xgb.DMatrix(tr$x, label = tr$y, missing = NA)
dte <- xgboost::xgb.DMatrix(te$x, missing = NA)

final_params <- list(
  objective        = "binary:logistic",
  eval_metric      = "aucpr",
  eta              = best$eta,
  max_depth        = best$max_depth,
  min_child_weight = best$min_child_weight,
  subsample        = best$subsample,
  colsample_bytree = best$colsample_bytree,
  lambda           = best$lambda,
  alpha            = best$alpha,
  scale_pos_weight = scale_pos_weight(tr$y),
  nthread          = 1,
  seed             = PROJECT_SEED
)

set_project_seed(PROJECT_SEED)
t_train_start <- proc.time()
final_model <- xgboost::xgb.train(
  params  = final_params,
  data    = dtr,
  nrounds = best$nrounds,
  verbose = 0
)
train_time_sec <- as.numeric((proc.time() - t_train_start)["elapsed"])

t_pred_start <- proc.time()
prob <- as.numeric(predict(final_model, dte))
pred_time_sec <- as.numeric((proc.time() - t_pred_start)["elapsed"])

stopifnot(
  length(prob) == nrow(test),
  all(is.finite(prob)),
  all(prob >= 0 & prob <= 1)
)
pred <- as.integer(prob >= best_threshold)

# Export standardized predictions for the paired model comparison.
write.csv(
  data.frame(
    sample_id      = test$obs_id,
    actual         = test$y,
    predicted      = pred,
    probability_ad = prob
  ),
  "outputs/predictions/xgboost_predictions.csv",
  row.names = FALSE
)

# In-sample training scores describe the optimism gap relative to OOF and test
# results. Diagnostic only: never used for selection, tuning or comparison.
prob_insample <- as.numeric(predict(final_model, dtr))

# Export comprehensive metrics table (including runtime)
m_fit   <- cbind(Set = "Train_insample", score_metrics(train$y, prob_insample, best_threshold),
                 Train_Time_Sec = train_time_sec, Pred_Time_Sec = NA_real_)
m_oof   <- cbind(Set = "OOF_train",   score_metrics(train$y, oof, best_threshold),
                 Train_Time_Sec = train_time_sec, Pred_Time_Sec = NA_real_)
m_t050  <- cbind(Set = "Test_0.50",   score_metrics(test$y, prob, 0.50),
                 Train_Time_Sec = train_time_sec, Pred_Time_Sec = pred_time_sec)
m_tuned <- cbind(Set = "Test_tuned",  score_metrics(test$y, prob, best_threshold),
                 Train_Time_Sec = train_time_sec, Pred_Time_Sec = pred_time_sec)

metrics <- rbind(m_fit, m_oof, m_t050, m_tuned)
stopifnot(all(metrics$TP + metrics$FP + metrics$FN + metrics$TN ==
                c(nrow(train), nrow(train), nrow(test), nrow(test))))
write.csv(metrics, "outputs/tables/xgboost_metrics.csv", row.names = FALSE)

# Save model artifacts & reproducibility metadata
saveRDS(final_model, "outputs/models/xgboost_model.rds")
saveRDS(
  list(
    best_config     = best,
    best_threshold  = best_threshold,
    prep            = prep_final,
    cv_results      = cv_tab,
    params          = final_params,
    train_time_sec  = train_time_sec,
    pred_time_sec   = pred_time_sec,
    tuning_time_sec = tuning_time_sec,
    seed            = PROJECT_SEED,
    xgboost_version = as.character(packageVersion("xgboost"))
  ),
  "outputs/models/xgboost_params.rds"
)

# 5b. Reproducibility metadata -------------------------------------------------
# Record the training protocol and bind this run to the shared input hashes.
alignment_paths <- c("data/processed/train_raw.rds",
                     "data/processed/test_raw.rds",
                     "data/processed/cv_folds.rds")
stopifnot(all(file.exists(alignment_paths)))

saveRDS(
  list(
    model                               = "XGBoost",
    owner                               = "Thanh Tai (P5)",
    training_only_preprocessing         = TRUE,
    shared_cv_folds_used                = TRUE,
    threshold_selected_on_training_only = TRUE,
    test_used_for_tuning                = FALSE,
    threshold                           = best_threshold,
    threshold_rule                      = ">=",
    threshold_criterion                 = "first max OOF F1 over seq(0.10, 0.90, by = 0.01)",
    seed                                = PROJECT_SEED,
    cv_folds                            = folds$k,
    cv_seed_rule                        = "PROJECT_SEED + config * 10 + fold",
    input_md5                           = tools::md5sum(alignment_paths),
    train_ids                           = as.integer(train$obs_id),
    test_ids                            = as.integer(test$obs_id),
    cv_fold_id                          = as.integer(folds$fold_id),
    timing_seconds                      = list(
      Training_time_s   = train_time_sec,
      Prediction_time_s = pred_time_sec,
      Tuning_time_s     = tuning_time_sec
    ),
    # Define tuning, final-refit and test-prediction timing boundaries explicitly.
    timing_boundaries = paste(
      "Training_time_s is the final XGBoost model refit (selected config) on",
      "the full training set; Prediction_time_s is prediction on the locked",
      "test set; neither is a CV fold time. Tuning_time_s is the total",
      "wall-clock time of the 6-config x 5-fold CV grid search."
    ),
    software_versions = c(R       = as.character(getRversion()),
                          xgboost = as.character(packageVersion("xgboost"))),
    run_timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  ),
  "outputs/models/xgboost_run_metadata.rds"
)

# Export figures as PNG for viewing and PDF for the report.
save_fig <- function(name, plot, width, height) {
  ggsave(file.path("outputs/figures", paste0(name, ".png")), plot,
         width = width, height = height, dpi = 300)
  ggsave(file.path("outputs/figures", paste0(name, ".pdf")), plot,
         width = width, height = height)
}

# 6. Feature Importance (Top 20 by Gain) ---------------------------------------
imp <- tryCatch(
  xgboost::xgb.importance(feature_names = colnames(tr$x), model = final_model),
  error = function(e) xgboost::xgb.importance(model = final_model)
)
imp_df <- as.data.frame(imp)
write.csv(imp_df, "outputs/tables/xgboost_importance.csv", row.names = FALSE)

top_n <- min(20L, nrow(imp_df))
imp_top <- imp_df[seq_len(top_n), ]
imp_top$Feature <- factor(imp_top$Feature, levels = rev(imp_top$Feature))

p_imp <- ggplot(imp_top, aes(x = Feature, y = Gain)) +
  geom_col(fill = "#2B6CB0", width = 0.72) +
  coord_flip() +
  labs(
    title    = sprintf("Top %d Feature Importance - XGBoost", top_n),
    subtitle = "Ranked by fractional contribution to tree splits (Gain)",
    x        = NULL,
    y        = "Relative Gain"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

save_fig("fig_xgboost_feature_importance", p_imp, width = 8, height = 6)

# 7. Publication-quality ggplot2 Visualizations --------------------------------
# 7a. Confusion Matrix
cm_df <- as.data.frame(table(
  Actual    = factor(test$y, levels = c(0, 1), labels = c("nonad (0)", "ad (1)")),
  Predicted = factor(pred,   levels = c(0, 1), labels = c("nonad (0)", "ad (1)"))
))

p_cm <- ggplot(cm_df, aes(x = Predicted, y = Actual, fill = Freq)) +
  geom_tile(color = "white", linewidth = 1.2) +
  geom_text(aes(label = Freq), size = 6, fontface = "bold") +
  scale_fill_gradient(low = "#EBF3FA", high = "#2B6CB0") +
  labs(
    title    = "XGBoost Confusion Matrix (Test Set)",
    subtitle = sprintf("Threshold = %.2f | F1 = %.4f | PR-AUC = %.4f",
                       best_threshold, m_tuned$F1_Score, m_tuned$PR_AUC),
    x        = "Predicted Label",
    y        = "Actual Label"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none", plot.title = element_text(face = "bold"))

save_fig("fig_xgboost_confusion_matrix", p_cm, width = 6.5, height = 5)

# 7b. ROC Curve
ord  <- order(prob, decreasing = TRUE)
yord <- test$y[ord]
tp_c <- cumsum(yord == 1); fp_c <- cumsum(yord == 0)
roc_df <- data.frame(
  FPR = c(0, fp_c / sum(test$y == 0)),
  TPR = c(0, tp_c / sum(test$y == 1))
)

p_roc <- ggplot(roc_df, aes(x = FPR, y = TPR)) +
  geom_line(color = "#2B6CB0", linewidth = 1.1) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
  labs(
    title = sprintf("XGBoost Test ROC Curve (AUC = %.4f)", m_tuned$ROC_AUC),
    x     = "False Positive Rate (1 - Specificity)",
    y     = "True Positive Rate (Recall)"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

save_fig("fig_xgboost_roc_curve", p_roc, width = 6.5, height = 5)

# 7c. Precision-Recall Curve
pr_df <- data.frame(
  Recall    = c(0, tp_c / sum(test$y == 1)),
  Precision = c(1, tp_c / (tp_c + fp_c))
)

p_pr <- ggplot(pr_df, aes(x = Recall, y = Precision)) +
  geom_step(color = "#C53030", linewidth = 1.1) +
  geom_hline(yintercept = mean(test$y == 1), linetype = "dashed", color = "gray50") +
  labs(
    title = sprintf("XGBoost Test Precision-Recall Curve (PR-AUC = %.4f)", m_tuned$PR_AUC),
    x     = "Recall",
    y     = "Precision"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

save_fig("fig_xgboost_pr_curve", p_pr, width = 6.5, height = 5)

# 8. False positives and false negatives --------------------------------------
# Same column layout as P4's random_forest_prediction_errors.csv so the two
# error profiles can be compared row by row in the discussion.
wrong  <- test$y != pred
errors <- data.frame(
  sample_id      = test$obs_id[wrong],
  actual         = test$y[wrong],
  predicted      = pred[wrong],
  probability_ad = prob[wrong],
  error_type     = ifelse(test$y[wrong] == 1L, "False negative", "False positive"),
  geom_missing   = as.integer(rowSums(is.na(test[wrong, NUMERIC_VARS, drop = FALSE])) > 0),
  local_missing  = as.integer(is.na(test$local[wrong])),
  local          = test$local[wrong],
  stringsAsFactors = FALSE
)
for (v in NUMERIC_VARS) errors[[v]] <- test[[v]][wrong]
errors <- errors[order(errors$error_type, -errors$probability_ad), ]
rownames(errors) <- NULL
write.csv(errors, "outputs/tables/xgboost_prediction_errors.csv", row.names = FALSE)

safe_mean <- function(x) if (length(x)) mean(x) else NA_real_
error_summary <- do.call(rbind, lapply(c("False negative", "False positive"), function(type) {
  ix <- errors$error_type == type
  data.frame(
    Error_type           = type,
    Count                = sum(ix),
    Mean_probability_ad  = safe_mean(errors$probability_ad[ix]),
    Any_geometry_missing = sum(errors$geom_missing[ix]),
    Local_missing        = sum(errors$local_missing[ix]),
    stringsAsFactors     = FALSE
  )
}))
write.csv(error_summary, "outputs/tables/xgboost_error_summary.csv", row.names = FALSE)
cat(sprintf("\nTest errors at threshold %.2f: %d false negatives, %d false positives\n",
            best_threshold, error_summary$Count[1], error_summary$Count[2]))

writeLines(capture.output(sessionInfo()), "outputs/tables/xgboost_session_info.txt")

print(cv_tab)
print(metrics)
print(error_summary)
cat("\nXGBoost outputs saved successfully!\n")
})
# END EMBEDDED SOURCE: R/09_xgboost.R

# BEGIN EMBEDDED SOURCE: R/10_model_comparison.R
.submission_modules[["R/10_model_comparison.R"]] <- quote({
# =============================================================================
# 10_model_comparison.R -- P4: performance and paired statistical comparison
# Author: Quynh Mai (P4)
# Execution: called automatically after all three models have been evaluated.
#
# Predictions are joined to the locked test observations by ID, never row order.
# Missing models remain explicitly pending. No model is retrained, and no test
# threshold is selected here. Existing classification labels are respected.
# =============================================================================

.submission_source("R/p4_helpers.R")
.submission_source("R/p4_xgboost_artifacts.R")
.submission_source("R/p4_logistic_artifacts.R")
.submission_source("R/p4_comparison_helpers.R")
shared <- p4_init()

model_files <- c(
  "Logistic Regression" = "logistic_predictions.csv",
  "Random Forest" = "random_forest_predictions.csv",
  "XGBoost" = "xgboost_predictions.csv"
)
metric_names <- c("Accuracy", "Precision", "Recall", "F1", "ROC_AUC", "PR_AUC")
model_colours <- c("Logistic Regression" = "#2a78d6", "Random Forest" = "#eb6834",
                  "XGBoost" = "#32936f")

run_metadata <- setNames(lapply(names(model_files), p4_read_run_metadata), names(model_files))
native_params_path <- "outputs/models/logistic_params.rds"
native_param_files <- native_params_path[file.exists(native_params_path)]
if (length(native_param_files)) {
  run_metadata[["Logistic Regression"]]$native_params <- tryCatch(
    readRDS(native_params_path), error = function(e) NULL)
}

predictions <- list()
availability <- vector("list", length(model_files))
for (i in seq_along(model_files)) {
  model <- names(model_files)[i]
  path <- file.path("outputs/predictions", model_files[[i]])
  if (file.exists(path)) {
    predictions[[model]] <- p4_read_predictions(path, shared$test)
    availability[[i]] <- data.frame(
      model = model, status = "available", predictions_file = path,
      n_test = nrow(predictions[[model]]),
      training_time_status = "pending audit", prediction_time_status = "pending audit",
      notes = "IDs, labels and score ranges verified; prediction CSV alone does not verify the training or threshold-selection protocol.",
      stringsAsFactors = FALSE
    )
  } else {
    availability[[i]] <- data.frame(
      model = model, status = "pending", predictions_file = path, n_test = NA_integer_,
      training_time_status = "pending", prediction_time_status = "pending",
      notes = "No prediction file: excluded from metrics and statistical comparisons.",
      stringsAsFactors = FALSE
    )
  }
}
availability <- do.call(rbind, availability)
native_checks <- list("Logistic Regression" = p4_verify_logistic_artifacts(
  shared, predictions[["Logistic Regression"]]))
if (is.list(run_metadata[["Logistic Regression"]]$native_params)) {
  attr(run_metadata[["Logistic Regression"]]$native_params, "p4_native_verified") <-
    isTRUE(native_checks[["Logistic Regression"]]$verified)
  attr(run_metadata[["Logistic Regression"]]$native_params, "p4_native_path") <- native_params_path
}
model_times <- setNames(lapply(names(model_files), function(model) {
  p4_read_model_times(model, run_metadata[[model]]$data,
                      native_params = run_metadata[[model]]$native_params)
}), names(model_files))
for (i in which(availability$status == "available")) {
  times <- model_times[[availability$model[i]]]$values
  availability$training_time_status[i] <- if (is.finite(times["Training_time_s"])) "available" else "not exported"
  availability$prediction_time_status[i] <- if (is.finite(times["Prediction_time_s"])) "available" else "not exported"
}
p4_write_table(availability, "comparison_availability", "Availability of the three model outputs")
if (!length(predictions)) stop("No valid model predictions are available.", call. = FALSE)

complete <- length(predictions) == length(model_files)
missing_models <- availability$model[availability$status == "pending"]
readiness_result <- p4_comparison_readiness(names(model_files), predictions, shared,
                                           model_times, run_metadata, native_checks = native_checks)
readiness <- readiness_result$table
selection_ready <- complete && all(readiness$final_selection_ready)
comparison_status <- if (!complete) paste0("partial predictions; pending: ", paste(missing_models, collapse = ", ")) else
  if (!selection_ready) "three-model numerical comparison complete; reproducibility documentation incomplete" else
    "all three prediction files and required final-selection evidence available"
p4_write_table(readiness, "comparison_readiness", "Prediction and run-documentation checks",
               note = "Numerical comparison is separate from complete run documentation. Native Logistic and XGBoost artifacts are checked directly; historical fold/input hashes are not inferred from prediction CSVs.")
runtime_sources <- do.call(rbind, lapply(model_times, function(x) x$sources))
p4_write_manifest(runtime_sources, "comparison_runtime_sources")
message("[P4] Comparison status: ", comparison_status)

# Recompute every model with the same implementation. PR-AUC uses a trapezoidal
# precision-recall curve with equal-score observations added as one group.
metrics <- do.call(rbind, lapply(names(predictions), function(model) {
  p <- predictions[[model]]
  values <- p4_metrics(p$actual, p$probability_ad, predicted = p$predicted)
  times <- model_times[[model]]$values
  data.frame(model = model, n_test = nrow(p), as.list(values), as.list(times),
             row.names = NULL, check.names = FALSE, stringsAsFactors = FALSE)
}))
p4_write_table(metrics, "comparison_metrics", "Performance on the same locked test observations",
               note = paste("Positive class: ad = 1. All metrics are recomputed consistently; PR-AUC is tie-grouped trapezoidal area.",
                            "Missing runtimes are not imputed. Timings from different runs or machines are descriptive only.",
                            comparison_status))

B <- suppressWarnings(as.integer(Sys.getenv("P4_BOOTSTRAP_REPS", "2000")))
if (length(B) != 1L || is.na(B) || B < 100L) stop("P4_BOOTSTRAP_REPS must be at least 100.")
boot <- p4_bootstrap(predictions, B = B, seed = PROJECT_SEED)
p4_write_table(boot$intervals, "comparison_bootstrap_ci", "Percentile bootstrap 95 percent confidence intervals",
               note = paste(B, "ordinary test-observation bootstrap resamples; the same sampled IDs are used for every model.",
                            "These intervals describe test-sample uncertainty conditional on the fitted models, not retraining variability."))
p4_write_table(boot$differences, "comparison_paired_differences", "Paired bootstrap differences in test performance",
               note = paste("Differences are model A minus model B. Marginal 95 percent percentile intervals;",
                            "approximate centred-null bootstrap p-values with Holm adjustment across model pairs within each metric.",
                            "Intervals are primary descriptive evidence and do not replace a practical-effect assessment."))

# McNemar's exact conditional test concerns paired classification errors; it is
# not a test of ROC-AUC, PR-AUC or F1. For zero discordances, p = 1.
mcnemar <- p4_exact_mcnemar(predictions)
p4_write_table(mcnemar, "comparison_mcnemar", "Exact paired McNemar tests with Holm adjustment",
               note = "The null hypothesis is equal discordant error probabilities. This does not directly test F1 or PR-AUC.")
diagnostics <- p4_pairwise_diagnostics(predictions, shared$test)
p4_write_table(diagnostics$paired_errors, "paired_error_summary", "Paired classification outcomes within each true class",
               note = "Counts are descriptive at the exported labels. Each class is partitioned into both correct, both wrong, A only correct and B only correct.")
p4_write_manifest(diagnostics$disagreements, "comparison_disagreements")
p4_write_table(diagnostics$cost_tradeoffs, "comparison_cost_tradeoffs", "Descriptive fixed-label false-positive/false-negative cost sensitivity",
               note = "The crossover is c_FN/c_FP and is reported only for opposing FP/FN changes. Costs are unspecified; this is not threshold optimization or a final model recommendation.")

# An explicitly provisional descriptive ordering. A complete comparative
# conclusion also needs uncertainty, robustness, interpretability and cost.
ranking <- metrics[order(-metrics$PR_AUC, -metrics$Recall, -metrics$F1),
                   c("model", "PR_AUC", "Recall", "F1", "Training_time_s", "Prediction_time_s"), drop = FALSE]
ranking$rank <- seq_len(nrow(ranking))
ranking$status <- if (!complete) paste("provisional; predictions pending:", paste(missing_models, collapse = ", ")) else
  if (!selection_ready) "descriptive three-model ranking; run documentation incomplete" else
    "descriptive ranking; assess uncertainty and operational costs before final selection"
p4_write_table(ranking, "comparison_provisional_ranking", "Descriptive model ordering by PR-AUC, then Recall and F1",
               note = "A point-estimate ranking is not evidence of a statistically or practically important difference.")

curve_points <- function(p) {
  ordering <- order(p$probability_ad, decreasing = TRUE)
  scores <- p$probability_ad[ordering]
  y <- p$actual[ordering]
  endpoints <- cumsum(rle(scores)$lengths)
  tp <- cumsum(y == 1L)[endpoints]
  fp <- cumsum(y == 0L)[endpoints]
  data.frame(fpr = c(0, fp / sum(y == 0L)), recall = c(0, tp / sum(y == 1L)),
             precision = c(1, tp / (tp + fp)))
}
curves <- lapply(predictions, curve_points)

p4_plot("fig_comparison_metrics", function() {
  par(mar = c(5, 4.2, 3.2, 1), xpd = FALSE)
  values <- t(as.matrix(metrics[, metric_names, drop = FALSE]))
  colnames(values) <- metrics$model
  barplot(t(values), beside = TRUE, ylim = c(0, 1.15),
          col = unname(model_colours[metrics$model]), border = NA,
          names.arg = c("Accuracy", "Precision", "Recall", "F1", "ROC-AUC", "PR-AUC"),
          ylab = "Held-out test performance", main = "Available-model performance")
  abline(h = seq(0, 1, 0.2), col = "#d8d8d8", lty = 3)
  legend("top", legend = metrics$model, fill = model_colours[metrics$model],
         bty = "n", horiz = TRUE, cex = 0.8)
}, width = 8.5, height = 4.5)

p4_plot("fig_comparison_curves", function() {
  par(mfrow = c(1, 2), mar = c(4.5, 4.5, 3, 1))
  plot(NA, xlim = c(0, 1), ylim = c(0, 1), xlab = "False positive rate",
       ylab = "Recall", main = "ROC curves", xaxs = "i", yaxs = "i")
  abline(0, 1, col = "grey70", lty = 2)
  for (model in names(curves)) lines(curves[[model]]$fpr, curves[[model]]$recall,
                                      col = model_colours[model], lwd = 2)
  legend("bottomright", names(curves), col = model_colours[names(curves)],
         lty = 1, lwd = 2, bty = "n", cex = 0.7)
  plot(NA, xlim = c(0, 1), ylim = c(0, 1), xlab = "Recall", ylab = "Precision",
       main = "Precision-recall curves", xaxs = "i", yaxs = "i")
  abline(h = mean(shared$test$y), col = "grey70", lty = 2)
  for (model in names(curves)) lines(curves[[model]]$recall, curves[[model]]$precision,
                                      col = model_colours[model], lwd = 2)
  legend("left", c(names(curves), "Ad prevalence"),
         col = c(model_colours[names(curves)], "grey70"),
         lty = c(rep(1, length(curves)), 2), lwd = 2, bty = "n", cex = 0.7,
         inset = 0.02)
}, width = 8.5, height = 4.2)

p4_plot("fig_comparison_intervals", function() {
  par(mfrow = c(1, 2), mar = c(4.5, 7, 3, 1))
  for (metric in c("PR_AUC", "Recall")) {
    tab <- boot$intervals[boot$intervals$metric == metric, , drop = FALSE]
    positions <- seq_len(nrow(tab))
    bounds <- range(c(tab$lower, tab$upper), finite = TRUE)
    bounds <- pmax(0, pmin(1, bounds + c(-0.025, 0.025)))
    if (diff(bounds) < 0.02) bounds <- c(max(0, bounds[1] - 0.02), min(1, bounds[2] + 0.02))
    plot(tab$estimate, positions, xlim = bounds, ylim = c(0.5, nrow(tab) + 0.5),
         yaxt = "n", xlab = "Test estimate and 95% percentile CI", ylab = "",
         main = gsub("_", "-", metric), pch = 19, col = model_colours[tab$model])
    axis(2, at = positions, labels = tab$model, las = 1, cex.axis = 0.75)
    segments(tab$lower, positions, tab$upper, positions, col = model_colours[tab$model], lwd = 2)
    points(tab$estimate, positions, pch = 19, col = model_colours[tab$model])
  }
}, width = 9, height = 4)

input_files <- c("data/processed/test_raw.rds", "data/processed/train_raw.rds", "data/processed/cv_folds.rds",
                 "R/10_model_comparison.R", "R/p4_helpers.R", "R/p4_comparison_helpers.R", "R/p4_xgboost_artifacts.R",
                 "R/p4_logistic_artifacts.R", native_param_files,
                 file.path("outputs/predictions", model_files[names(predictions)]),
                 unlist(lapply(model_times, function(x) x$input_files), use.names = FALSE),
                 readiness_result$input_files)
input_files <- unique(gsub("\\\\", "/", input_files))
input_files <- unique(c(.submission_input_paths(input_files), .submission_file, .submission_csv))
stopifnot(all(file.exists(input_files)))
input_md5 <- setNames(unname(tools::md5sum(input_files)), input_files)
comparison <- list(
  status = comparison_status, all_models_available = complete,
  final_selection_ready = selection_ready, readiness = readiness,
  availability = availability, metrics = metrics, intervals = boot$intervals,
  paired_differences = boot$differences, mcnemar = mcnemar, ranking = ranking,
  paired_errors = diagnostics$paired_errors, disagreements = diagnostics$disagreements,
  cost_tradeoffs = diagnostics$cost_tradeoffs, runtime_sources = runtime_sources,
  native_logistic_metadata = native_checks[["Logistic Regression"]]$metadata,
  input_md5 = input_md5,
  bootstrap_reps = B, bootstrap_seed = PROJECT_SEED,
  locked_test_ids = shared$test$obs_id,
  notes = c("PR-AUC uses grouped equal-score thresholds and trapezoidal area.",
            "Paired resampling retains the same test IDs for every model.",
            "Uncertainty is conditional on fitted models; no training resampling is performed.",
            "Existing P3 probabilities were originally exported at six decimal places; current prediction files are hashed.",
            "Native XGBoost model/preprocessing/OOF consistency is checked directly; absent historical input hashes are not invented.",
            if (isTRUE(native_checks[["Logistic Regression"]]$verified))
              "Native weighted ridge Logistic (alpha = 0), lambda/threshold, model predictions and exported OOF consistency were verified; preprocessing was reconstructed from current training data and historical CV fits were not rerun." else
                "Native Logistic artifact verification is incomplete; see comparison readiness for the specific checks still missing.",
            "Audited Logistic timing boundaries: final model fit and locked-test response exclude preprocessing; 5-fold tuning includes fold preprocessing, fitting and OOF prediction, excluding the later threshold scan.",
            "Zero exported prediction seconds indicate the timer's resolution, not zero computational cost.",
            "Cost crossovers are descriptive at fixed exported labels and do not tune thresholds or select a final model.",
            "No test-driven threshold tuning or definitive model selection is performed.")
)
saveRDS(comparison, "outputs/models/p4_comparison.rds")
status_lines <- c("P4 comparison status", comparison_status,
             paste("Available:", paste(names(predictions), collapse = ", ")),
             paste("Pending prediction files:", if (length(missing_models)) paste(missing_models, collapse = ", ") else "none"),
             paste("Full run-documentation evidence ready:", selection_ready),
             if (any(!readiness$final_selection_ready)) paste0(
               readiness$model[!readiness$final_selection_ready], ": ",
               readiness$missing_details[!readiness$final_selection_ready]),
             unlist(lapply(model_times, function(x) x$notes), use.names = FALSE),
             "Rerun R/10_model_comparison.R when prediction files, runtime metadata or provider artifacts change.")
writeLines(status_lines, "outputs/tables/comparison_status.txt")
message("[P4] Saved metrics, paired bootstrap intervals, exact McNemar tests and comparison figures.")
})
# END EMBEDDED SOURCE: R/10_model_comparison.R

# BEGIN EMBEDDED SOURCE: R/11_robustness_analysis.R
.submission_modules[["R/11_robustness_analysis.R"]] <- quote({
# =============================================================================
# 11_robustness_analysis.R -- P4: Random Forest seed/threshold sensitivity
# Author: Quynh Mai (P4)
# Execution: called automatically after the baseline Random Forest fit.
#
# The training observations, five validation folds, chosen hyperparameters,
# preprocessing options, and classification threshold stay fixed. Only the RF
# training seed changes. Threshold sensitivity uses training OOF predictions;
# the locked test set is never used to select thresholds or seeds.
# =============================================================================

.submission_source("R/p4_helpers.R")
.submission_source("R/p4_rf_functions.R")

# This function has the global environment rather than the enclosing analysis
# environment, so PSOCK serializes only its explicit task/parameter arguments.
p4_robustness_fit_fold <- function(task, params, seed) {
  started <- proc.time()[[3L]]
  fit <- p4_rf_fit(task$fd$analysis, params, seed = seed + task$k - 1L)
  probability <- p4_rf_probability(fit, task$fd$assessment)
  list(k = task$k, probability = probability,
       elapsed_s = proc.time()[[3L]] - started,
       effective_mtry = fit$p4_effective_mtry)
}

p4_run_robustness <- function() {
shared_robust <- p4_init()
if (!requireNamespace("randomForest", quietly = TRUE)) {
  stop("Install the 'randomForest' package before running RF robustness.", call. = FALSE)
}

robust_params_path <- "outputs/models/random_forest_params.rds"
robust_oof_path <- "outputs/predictions/random_forest_oof_predictions.csv"
if (!all(file.exists(c(robust_params_path, robust_oof_path)))) {
  stop("Run R/08_random_forest.R first: RF parameters and OOF predictions are required.",
       call. = FALSE)
}

robust_params <- readRDS(robust_params_path)
robust_required <- c("ntree", "mtry", "nodesize", "threshold", "seed", "opts")
if (!is.list(robust_params) || !all(robust_required %in% names(robust_params))) {
  stop("RF parameters must include: ", paste(robust_required, collapse = ", "),
       call. = FALSE)
}
if (!is.finite(robust_params$threshold) || robust_params$threshold < 0 ||
    robust_params$threshold > 1) {
  stop("Invalid RF validation-selected threshold.", call. = FALSE)
}

robust_train <- shared_robust$train
robust_folds <- shared_robust$folds
robust_base <- p4_read_predictions(robust_oof_path, robust_train)
robust_fold_id <- integer(nrow(robust_train))
for (robust_k in seq_len(robust_folds$k)) {
  if (anyDuplicated(robust_folds$index[[robust_k]]) ||
      !setequal(robust_folds$index[[robust_k]],
                setdiff(seq_len(nrow(robust_train)), robust_folds$indexOut[[robust_k]]))) {
    stop("A shared training fold is not the complement of its validation fold.", call. = FALSE)
  }
  robust_fold_id[robust_folds$indexOut[[robust_k]]] <- robust_k
}
if (any(robust_fold_id == 0L) ||
    anyDuplicated(unlist(robust_folds$indexOut)) ||
    !identical(sort(unname(unlist(robust_folds$indexOut))), seq_len(nrow(robust_train)))) {
  stop("Shared validation folds must cover every training row exactly once.", call. = FALSE)
}
robust_base_metadata <- read.csv(robust_oof_path, stringsAsFactors = FALSE)
robust_id_column <- if ("sample_id" %in% names(robust_base_metadata)) "sample_id" else "obs_id"
robust_base_metadata <- robust_base_metadata[
  match(robust_train$obs_id, robust_base_metadata[[robust_id_column]]), , drop = FALSE]
if (!"fold" %in% names(robust_base_metadata) ||
    anyNA(robust_base_metadata$fold) ||
    !identical(as.integer(robust_base_metadata$fold), robust_fold_id)) {
  stop("Reused RF OOF fold labels do not match the shared validation folds.", call. = FALSE)
}
if ("seed" %in% names(robust_base_metadata) &&
    (anyNA(robust_base_metadata$seed) ||
     any(robust_base_metadata$seed != robust_params$seed))) {
  stop("Reused RF OOF seed metadata disagrees with the saved baseline seed.", call. = FALSE)
}
if (!is.null(robust_params$cv_seed_rule) &&
    !identical(robust_params$cv_seed_rule, "seed + fold - 1")) {
  stop("RF baseline uses a different per-fold training seed convention.", call. = FALSE)
}
if (!is.null(robust_params$cv_folds) && robust_params$cv_folds != robust_folds$k) {
  stop("RF baseline parameters refer to a different number of CV folds.", call. = FALSE)
}
if (any(robust_base$predicted !=
        as.integer(robust_base$probability_ad >= robust_params$threshold))) {
  stop("RF OOF predicted labels disagree with the saved threshold.", call. = FALSE)
}

robust_metric_names <- c("Accuracy", "Precision", "Recall", "Specificity", "F1",
                         "ROC_AUC", "PR_AUC")
robust_seeds <- unique(as.integer(c(robust_params$seed, 123L, 2026L)))
robust_seed_rows <- list()
robust_fold_rows <- list()
robust_prediction_rows <- list()
robust_started <- proc.time()[[3L]]

# These are deterministic, fold-specific transforms. Reusing them across seeds
# changes only the forest RNG and never fits preprocessing on a validation fold.
message("[P4] Preparing the same five training-only preprocessing folds...")
robust_data <- lapply(seq_len(robust_folds$k), function(k) {
  cv_fold_data(k, robust_train, robust_folds, robust_params$opts, target = "factor")
})
robust_tasks <- lapply(seq_len(robust_folds$k), function(k) list(k = k, fd = robust_data[[k]]))
robust_workers <- p4_rf_worker_count(robust_folds$k)
robust_cluster <- NULL
on.exit({
  if (!is.null(robust_cluster)) try(parallel::stopCluster(robust_cluster), silent = TRUE)
}, add = TRUE)
if (robust_workers > 1L) {
  message("[P4] Starting ", robust_workers, " PSOCK workers for fixed-fold seed sensitivity...")
  robust_cluster <- p4_rf_make_cluster(robust_workers)
}

for (robust_seed in robust_seeds) {
  message("[P4] Fixed-fold RF robustness: seed ", robust_seed)
  robust_probs <- rep(NA_real_, nrow(robust_train))
  robust_elapsed <- NA_real_
  robust_fold_elapsed <- rep(NA_real_, robust_folds$k)
  if (robust_seed == as.integer(robust_params$seed)) {
    # Reuse selected-config OOF predictions; no extra seed-42 fits are needed.
    robust_probs <- robust_base$probability_ad
  } else {
    robust_seed_started <- proc.time()[[3L]]
    if (is.null(robust_cluster)) {
      robust_results <- lapply(robust_tasks, p4_robustness_fit_fold,
                                params = robust_params, seed = robust_seed)
    } else {
      robust_results <- parallel::parLapplyLB(robust_cluster, robust_tasks,
                                              p4_robustness_fit_fold,
                                              params = robust_params, seed = robust_seed)
    }
    for (robust_result in robust_results) {
      robust_k <- robust_result$k
      robust_validation <- robust_folds$indexOut[[robust_k]]
      robust_probs[robust_validation] <- robust_result$probability
      robust_fold_elapsed[robust_k] <- robust_result$elapsed_s
    }
    robust_elapsed <- proc.time()[[3L]] - robust_seed_started
  }
  for (robust_k in seq_len(robust_folds$k)) {
    robust_fd <- robust_data[[robust_k]]
    robust_validation <- robust_folds$indexOut[[robust_k]]
    robust_p <- ncol(robust_fd$analysis) - 1L
    robust_mtry <- max(1L, min(as.integer(robust_params$mtry), robust_p))
    robust_fm <- p4_metrics(robust_train$y[robust_validation],
                            robust_probs[robust_validation],
                            threshold = robust_params$threshold)
    robust_fold_rows[[length(robust_fold_rows) + 1L]] <- data.frame(
      seed = robust_seed, fold = robust_k,
      n_validation = length(robust_validation), predictors = robust_p,
      mtry = robust_mtry, threshold = robust_params$threshold,
      elapsed_s = robust_fold_elapsed[robust_k],
      as.list(robust_fm[robust_metric_names]), check.names = FALSE
    )
  }
  if (anyNA(robust_probs)) stop("Incomplete robustness OOF predictions.", call. = FALSE)
  robust_sm <- p4_metrics(robust_train$y, robust_probs,
                         threshold = robust_params$threshold)
  robust_seed_rows[[length(robust_seed_rows) + 1L]] <- data.frame(
    seed = robust_seed, baseline = robust_seed == as.integer(robust_params$seed),
    threshold = robust_params$threshold, elapsed_s = robust_elapsed,
    as.list(robust_sm[robust_metric_names]), check.names = FALSE
  )
  robust_prediction_rows[[length(robust_prediction_rows) + 1L]] <- data.frame(
    sample_id = robust_train$obs_id, actual = robust_train$y,
    predicted = as.integer(robust_probs >= robust_params$threshold),
    probability_ad = robust_probs, fold = robust_fold_id, seed = robust_seed,
    threshold = robust_params$threshold
  )
}

robust_seed_metrics <- do.call(rbind, robust_seed_rows)
robust_fold_metrics <- do.call(rbind, robust_fold_rows)
robust_predictions <- do.call(rbind, robust_prediction_rows)
robust_summary <- do.call(rbind, lapply(robust_metric_names, function(metric) {
  vals <- robust_seed_metrics[[metric]]
  data.frame(metric = metric, n_seeds = length(vals), mean = mean(vals),
             sd = stats::sd(vals), minimum = min(vals), maximum = max(vals),
             range = diff(range(vals)))
}))

# A descriptive threshold sweep on the BASE training OOF scores. The saved
# baseline threshold remains the one chosen by the RF script before test use.
robust_thresholds <- do.call(rbind, lapply(seq(0.10, 0.90, by = 0.01), function(th) {
  m <- p4_metrics(robust_train$y, robust_base$probability_ad, threshold = th)
  data.frame(threshold = th,
             selected = abs(th - robust_params$threshold) < 1e-8,
             as.list(m[c("Accuracy", "Precision", "Recall", "Specificity", "F1")]),
             check.names = FALSE)
}))

robust_note <- paste0(
  "RF only; identical training observations and shared validation folds; ",
  "hyperparameters and threshold fixed from the baseline OOF run. ",
  "Baseline OOF predictions are reused, so its elapsed time is unavailable. ",
  "Three seeds describe RNG sensitivity and are not an independent test or confidence interval."
)
p4_write_table(robust_seed_metrics, "robustness_seed_metrics",
               "Random Forest performance across training seeds (pooled fixed-fold OOF predictions)",
               note = robust_note)
p4_write_table(robust_summary, "robustness_seed_summary",
               "Descriptive Random Forest seed-sensitivity summary", note = robust_note)
p4_write_table(robust_fold_metrics, "robustness_fold_metrics",
               "Random Forest validation-fold metrics across training seeds",
               note = "Overlapping training sets mean these fold scores are not independent observations.")
p4_write_table(robust_thresholds, "robustness_thresholds",
               "Random Forest classification-threshold sensitivity on baseline training OOF predictions",
               note = "The sweep is descriptive; the locked test set is not used for threshold selection.")
write.csv(robust_predictions, "outputs/predictions/random_forest_robustness_oof_predictions.csv",
          row.names = FALSE)

p4_plot("fig_rf_seed_sensitivity", function() {
  metrics <- c("Recall", "F1", "ROC_AUC", "PR_AUC")
  vals <- as.matrix(robust_seed_metrics[, metrics, drop = FALSE])
  colours <- c("#eb6834", "#2a78d6", "#5c8b45", "#8a5aa5")
  old <- par(mar = c(4.5, 4.2, 5.0, 1))
  on.exit(par(old), add = TRUE)
  matplot(seq_along(robust_seeds), vals, type = "b", pch = seq_along(metrics),
          lty = 1, col = colours, xaxt = "n",
          ylim = c(max(0, min(vals) - 0.03), min(1, max(vals) + 0.03)),
          xlab = "RF training seed (fixed split and folds)", ylab = "OOF score",
          main = "")
  axis(1, at = seq_along(robust_seeds), labels = robust_seed_metrics$seed)
  title(main = "Random Forest seed sensitivity", line = 2.3)
  legend("top", inset = c(0, -0.16), horiz = TRUE, xpd = NA,
         legend = gsub("_", "-", metrics), col = colours,
         pch = seq_along(metrics), lty = 1, bty = "n", cex = 0.9)
})
p4_plot("fig_rf_threshold_sensitivity", function() {
  metrics <- c("Precision", "Recall", "F1")
  colours <- c("#2a78d6", "#eb6834", "#5c8b45")
  old <- par(mar = c(4.5, 4.2, 3.3, 1))
  on.exit(par(old), add = TRUE)
  matplot(robust_thresholds$threshold,
          as.matrix(robust_thresholds[, metrics, drop = FALSE]),
          type = "l", lty = 1, lwd = 2, col = colours, ylim = c(0, 1),
          xlab = "Classification threshold", ylab = "Training OOF score",
          main = "Random Forest threshold sensitivity")
  abline(v = robust_params$threshold, lty = 2, col = "#444444")
  legend("bottomleft", legend = c(metrics, sprintf("Selected: %.2f", robust_params$threshold)),
         col = c(colours, "#444444"), lty = c(1, 1, 1, 2), lwd = c(2, 2, 2, 1),
         bty = "n", cex = 0.9)
})

robust_bundle <- list(
  model = "Random Forest", scope = "training-only fixed-fold RNG and threshold sensitivity",
  params = robust_params, seeds = robust_seeds,
  workers = robust_workers,
  training_ids = robust_train$obs_id, fold_id = robust_fold_id,
  seed_metrics = robust_seed_metrics, fold_metrics = robust_fold_metrics,
  seed_summary = robust_summary, thresholds = robust_thresholds,
  input_md5 = tools::md5sum(c("data/processed/train_raw.rds",
                              "data/processed/cv_folds.rds",
                              robust_params_path, robust_oof_path)),
  elapsed_s = proc.time()[[3L]] - robust_started,
  note = robust_note, session_info = utils::sessionInfo()
)
saveRDS(robust_bundle, "outputs/models/p4_robustness.rds")
message("[P4] RF robustness complete. No test observations were used in this analysis.")
invisible(robust_bundle)
}

p4_run_robustness()
})
# END EMBEDDED SOURCE: R/11_robustness_analysis.R

# BEGIN EMBEDDED SOURCE: R/13_p4_interpretation.R
.submission_modules[["R/13_p4_interpretation.R"]] <- quote({
# =============================================================================
# Retrospective P2 context for the already fitted P4 Random Forest.
# Execution: called automatically after descriptive statistics and model fitting.
# Reads existing P2 tables only; never sources P2 analysis scripts, fits a model,
# or changes predictors, hyperparameters, class weights or thresholds.
# =============================================================================
.submission_source("R/p4_helpers.R")

p4_run_interpretation <- function() {
  scope <- paste("retrospective full-sample context including test;",
                 "not independent validation; not used for model selection")
  input_paths <- c(
    clean = "data/processed/ad_clean.rds",
    dictionary = "data/processed/feature_dictionary.csv",
    classes = "outputs/tables/class_distribution.csv",
    descriptive = "outputs/tables/geometry_descriptive.csv",
    geometry_tests = "outputs/tables/test_a_geometry.csv",
    keyword_tests = "outputs/tables/test_b_keywords.csv",
    frequency = "outputs/tables/keyword_frequency.csv",
    importance = "outputs/tables/random_forest_feature_importance.csv"
  )
  for (path in c("outputs/tables", "outputs/models", "report/Part_4/tables")) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  missing <- unname(input_paths[!file.exists(input_paths)])
  if (length(missing)) {
    pending <- list(status = "pending", geometry = NULL, keywords = NULL,
                    input_md5 = tools::md5sum(unname(input_paths[file.exists(input_paths)])),
                    missing_inputs = missing, scope = scope)
    saveRDS(pending, "outputs/models/p4_interpretation.rds")
    p4_write_manifest(data.frame(status = "pending", scope = scope,
                                note = paste("Missing inputs:", paste(missing, collapse = "; "))),
                      "p4_interpretation_status")
    message("[P4] P2 interpretation pending; no context values were generated. Missing: ",
            paste(missing, collapse = ", "))
    return(invisible(pending))
  }

  read_table <- function(key) {
    read.csv(input_paths[[key]], stringsAsFactors = FALSE,
             check.names = FALSE, encoding = "UTF-8")
  }
  fail <- function(message) {
    stop("P2/P4 interpretation input mismatch: ", message,
         ". Regenerate the affected P2/P4 outputs from the same shared data before rerunning R/13_p4_interpretation.R.",
         call. = FALSE)
  }
  require_columns <- function(data, columns, label) {
    if (!all(columns %in% names(data))) fail(paste(label, "has missing columns"))
  }
  same_numeric <- function(actual, expected, label, tolerance = 1e-9) {
    if (!isTRUE(all.equal(as.numeric(actual), as.numeric(expected),
                          tolerance = tolerance, check.attributes = FALSE))) fail(label)
  }
  assert_features <- function(data, expected, label) {
    if (anyNA(data$feature) || anyDuplicated(data$feature) ||
        !setequal(data$feature, expected)) fail(paste(label, "has incompatible feature rows"))
    data[match(expected, data$feature), , drop = FALSE]
  }
  clean <- readRDS(input_paths[["clean"]])
  dictionary <- read_table("dictionary")
  counts <- read_table("classes")
  descriptive <- read_table("descriptive")
  test_a <- read_table("geometry_tests")
  test_b <- read_table("keyword_tests")
  frequency <- read_table("frequency")
  importance <- read_table("importance")
  require_columns(clean, c("obs_id", "y", NUMERIC_VARS), "ad_clean")
  if (anyNA(clean$y) || !all(clean$y %in% 0:1) || anyDuplicated(clean$obs_id)) {
    fail("ad_clean labels or observation IDs are invalid")
  }
  actual_counts <- c(all = nrow(clean), ad = sum(clean$y == 1L), nonad = sum(clean$y == 0L))
  same_numeric(actual_counts, c(2423L, 386L, 2037L), "expected deduplicated sample counts changed")
  require_columns(counts, c("Class", "N", "Share"), "class_distribution")
  if (anyDuplicated(counts$Class) || !setequal(counts$Class, c("ad", "nonad"))) {
    fail("class_distribution class rows are invalid")
  }
  counts <- counts[match(c("ad", "nonad"), counts$Class), , drop = FALSE]
  same_numeric(counts$N, actual_counts[c("ad", "nonad")], "class counts do not match ad_clean")
  same_numeric(counts$Share, counts$N / nrow(clean), "class shares do not match ad_clean")

  require_columns(dictionary, c("column", "group", "original_name"), "feature_dictionary")
  if (anyDuplicated(dictionary$column) || anyDuplicated(dictionary$original_name)) {
    fail("feature dictionary is not a one-to-one mapping")
  }
  # Some committed P2 CSVs serialize non-ASCII terms as literal <U+00AE>
  # sequences. Match that representation to this dictionary only; do not guess
  # feature identities or rewrite P2's files. Every mapping must be unambiguous.
  escaped_dictionary_names <- vapply(dictionary$original_name, function(name) {
    points <- utf8ToInt(enc2utf8(name))
    paste0(vapply(points, function(point) {
      if (point > 127L) sprintf("<U+%04X>", point) else intToUtf8(point)
    }, character(1)), collapse = "")
  }, character(1), USE.NAMES = FALSE)
  if (anyDuplicated(escaped_dictionary_names)) fail("escaped dictionary names are ambiguous")
  normalize_feature_names <- function(names) {
    escaped_match <- match(names, escaped_dictionary_names)
    replace <- !names %in% dictionary$original_name & !is.na(escaped_match)
    names[replace] <- dictionary$original_name[escaped_match[replace]]
    names
  }
  for (key in c("descriptive", "test_a", "test_b", "frequency", "importance")) {
    table <- get(key)
    if ("feature" %in% names(table)) table$feature <- normalize_feature_names(table$feature)
    assign(key, table)
  }
  keyword_dictionary <- dictionary[dictionary$group %in% BINARY_GROUPS, , drop = FALSE]
  if (!nrow(keyword_dictionary) ||
      !all(keyword_dictionary$column %in% names(clean))) fail("keyword dictionary does not match ad_clean")
  keyword_names <- keyword_dictionary$original_name
  groups <- list(all = seq_len(nrow(clean)), ad = which(clean$y == 1L),
                 nonad = which(clean$y == 0L))
  require_columns(descriptive, c("group", "feature", "n", "missing", "mean"),
                  "geometry_descriptive")
  require_columns(frequency, c("group", "feature", "observed", "present", "share"),
                  "keyword_frequency")
  if (!setequal(unique(descriptive$group), names(groups)) ||
      !setequal(unique(frequency$group), names(groups))) fail("P2 group definitions changed")

  # Validate every geometry observation count and all keyword frequencies, not
  # just the few features that will be displayed in the interpretation tables.
  frequency_by_group <- list()
  for (group in names(groups)) {
    rows <- groups[[group]]
    dg <- descriptive[descriptive$group == group, , drop = FALSE]
    dg <- assert_features(dg, NUMERIC_VARS, paste(group, "geometry descriptive"))
    observed <- vapply(NUMERIC_VARS, function(v) sum(!is.na(clean[[v]][rows])), integer(1))
    means <- vapply(NUMERIC_VARS, function(v) mean(clean[[v]][rows], na.rm = TRUE), numeric(1))
    same_numeric(dg$n, observed, paste(group, "geometry observation counts"))
    same_numeric(dg$missing, length(rows) - observed, paste(group, "geometry missing counts"))
    same_numeric(dg$mean, means, paste(group, "geometry means"))
    fg <- frequency[frequency$group == group, , drop = FALSE]
    fg <- assert_features(fg, keyword_names, paste(group, "keyword frequency"))
    observed <- vapply(keyword_dictionary$column, function(v) sum(!is.na(clean[[v]][rows])), integer(1))
    present <- vapply(keyword_dictionary$column, function(v) sum(clean[[v]][rows] == 1L, na.rm = TRUE), integer(1))
    same_numeric(fg$observed, observed, paste(group, "keyword observed counts"))
    same_numeric(fg$present, present, paste(group, "keyword presence counts"))
    same_numeric(fg$share, present / observed, paste(group, "keyword frequencies"))
    frequency_by_group[[group]] <- fg
  }

  require_columns(test_a, c("feature", "n_ad", "n_nonad", "difference", "ci_low", "ci_high", "p", "p_holm"),
                  "test_a_geometry")
  test_a <- assert_features(test_a, NUMERIC_VARS, "test_a_geometry")
  # These three inexpensive Welch computations validate the stored P2 results;
  # they do not fit a classifier or introduce new feature-selection decisions.
  expected_welch <- lapply(NUMERIC_VARS, function(v) {
    stats::t.test(clean[[v]][groups$ad], clean[[v]][groups$nonad])
  })
  expected_p <- vapply(expected_welch, function(x) x$p.value, numeric(1))
  same_numeric(test_a$n_ad, vapply(NUMERIC_VARS, function(v) sum(!is.na(clean[[v]][groups$ad])), integer(1)),
               "Welch ad observation counts")
  same_numeric(test_a$n_nonad, vapply(NUMERIC_VARS, function(v) sum(!is.na(clean[[v]][groups$nonad])), integer(1)),
               "Welch nonad observation counts")
  same_numeric(test_a$difference, vapply(expected_welch, function(x) unname(diff(rev(x$estimate))), numeric(1)),
               "Welch mean differences")
  same_numeric(test_a$ci_low, vapply(expected_welch, function(x) x$conf.int[1L], numeric(1)), "Welch CI lower bounds")
  same_numeric(test_a$ci_high, vapply(expected_welch, function(x) x$conf.int[2L], numeric(1)), "Welch CI upper bounds")
  same_numeric(test_a$p, expected_p, "Welch raw p-values")
  same_numeric(test_a$p_holm, stats::p.adjust(expected_p, method = "holm"), "Welch Holm p-values")

  require_columns(test_b, c("feature", "absent_nonad", "absent_ad", "present_nonad", "present_ad",
                            "method", "odds_ratio", "p", "p_bh"), "test_b_keywords")
  test_b <- assert_features(test_b, keyword_names, "test_b_keywords")
  same_numeric(test_b$present_ad, frequency_by_group$ad$present, "keyword-test ad presence counts")
  same_numeric(test_b$present_nonad, frequency_by_group$nonad$present, "keyword-test nonad presence counts")
  same_numeric(test_b$absent_ad, frequency_by_group$ad$observed - frequency_by_group$ad$present,
               "keyword-test ad absence counts")
  same_numeric(test_b$absent_nonad, frequency_by_group$nonad$observed - frequency_by_group$nonad$present,
               "keyword-test nonad absence counts")
  same_numeric(test_b$p_bh, stats::p.adjust(test_b$p, method = "BH"), "keyword BH p-values")

  require_columns(importance, c("column", "feature", "permutation_importance"), "RF importance")
  if (anyDuplicated(importance$column) || anyNA(importance$permutation_importance) ||
      any(!is.finite(importance$permutation_importance))) fail("RF importance is invalid")
  importance <- importance[order(-importance$permutation_importance, importance$column), , drop = FALSE]
  importance$rf_rank <- seq_len(nrow(importance))
  geometry_rows <- match(NUMERIC_VARS, importance$column)
  if (anyNA(geometry_rows)) fail("RF importance does not contain all three geometry variables")
  ad_descriptive <- descriptive[descriptive$group == "ad", , drop = FALSE]
  nonad_descriptive <- descriptive[descriptive$group == "nonad", , drop = FALSE]
  geometry <- data.frame(
    feature = NUMERIC_VARS, rf_rank = importance$rf_rank[geometry_rows],
    rf_permutation_importance = importance$permutation_importance[geometry_rows],
    n_ad = test_a$n_ad, n_nonad = test_a$n_nonad,
    mean_ad = ad_descriptive$mean[match(NUMERIC_VARS, ad_descriptive$feature)],
    mean_nonad = nonad_descriptive$mean[match(NUMERIC_VARS, nonad_descriptive$feature)],
    mean_difference = test_a$difference, ci_low = test_a$ci_low,
    ci_high = test_a$ci_high, p_holm = test_a$p_holm, stringsAsFactors = FALSE
  )
  keyword_importance <- importance[importance$column %in% keyword_dictionary$column, , drop = FALSE]
  keyword_importance <- head(keyword_importance, 6L)
  if (nrow(keyword_importance) != 6L) fail("fewer than six keyword importance values are available")
  original_names <- keyword_dictionary$original_name[match(keyword_importance$column, keyword_dictionary$column)]
  if (!identical(as.character(keyword_importance$feature), as.character(original_names))) {
    fail("RF keyword names do not match the current dictionary")
  }
  ix <- match(original_names, test_b$feature)
  keywords <- data.frame(
    feature = original_names, rf_rank = keyword_importance$rf_rank,
    rf_permutation_importance = keyword_importance$permutation_importance,
    observed_ad = frequency_by_group$ad$observed[ix], present_ad = frequency_by_group$ad$present[ix],
    frequency_ad = frequency_by_group$ad$share[ix],
    observed_nonad = frequency_by_group$nonad$observed[ix],
    present_nonad = frequency_by_group$nonad$present[ix],
    frequency_nonad = frequency_by_group$nonad$share[ix],
    odds_ratio = test_b$odds_ratio[ix], test_method = test_b$method[ix],
    p_bh = test_b$p_bh[ix], stringsAsFactors = FALSE
  )
  p_display <- function(x) {
    ifelse(is.na(x), "--", ifelse(x < 0.0001, "<0.0001", sprintf("%.4f", x)))
  }
  or_display <- function(x) {
    vapply(x, function(value) {
      if (is.na(value)) "--" else if (is.infinite(value)) "Inf" else sprintf("%.2f", value)
    }, character(1))
  }
  note <- paste("P2 uses the full deduplicated sample, including test observations; these are retrospective associations,",
                "not independent validation or inputs to feature/model/threshold selection.",
                "RF rank is based on training-forest permutation importance, which is not a causal effect or an odds ratio.")
  p4_write_manifest(geometry, "rf_geometry_context")
  p4_write_manifest(keywords, "rf_keyword_context")
  p4_write_table(data.frame(
    Feature = geometry$feature, `RF rank` = geometry$rf_rank,
    `Ad minus nonad mean` = sprintf("%.2f", geometry$mean_difference),
    `95% CI` = sprintf("[%.2f, %.2f]", geometry$ci_low, geometry$ci_high),
    `Holm p` = p_display(geometry$p_holm), check.names = FALSE
  ), "report_rf_geometry_context", "Geometric RF predictors and retrospective P2 mean comparisons", note)
  p4_write_table(data.frame(
    Feature = keywords$feature, `RF rank` = keywords$rf_rank,
    `Ad frequency (%)` = sprintf("%.2f", 100 * keywords$frequency_ad),
    `Nonad frequency (%)` = sprintf("%.2f", 100 * keywords$frequency_nonad),
    OR = or_display(keywords$odds_ratio), `BH q` = p_display(keywords$p_bh), check.names = FALSE
  ), "report_rf_keyword_context", "Leading RF keyword predictors and retrospective P2 associations", note)
  bundle <- list(status = "available", geometry = geometry, keywords = keywords,
                  input_md5 = tools::md5sum(unname(input_paths)), scope = scope,
                  sample_counts = actual_counts,
                  note = "Full-sample P2 context was checked against ad_clean. No classifier was fitted or modified.")
  saveRDS(bundle, "outputs/models/p4_interpretation.rds")
  p4_write_manifest(data.frame(status = "available", scope = scope,
                              note = "P2 sample counts, feature observations and stored tests passed consistency checks."),
                    "p4_interpretation_status")
  message("[P4] Saved retrospective P2/RF context for three geometric and six keyword predictors. No model was changed.")
  invisible(bundle)
}

p4_run_interpretation()
})
# END EMBEDDED SOURCE: R/13_p4_interpretation.R

# BEGIN EMBEDDED SOURCE: R/14_p4_threshold_sensitivity.R
.submission_modules[["R/14_p4_threshold_sensitivity.R"]] <- quote({
# P4: descriptive threshold sensitivity from saved TRAINING OOF predictions.
# Execution: called automatically after the model OOF exports are available.
# No model is fitted, no threshold is selected, and no test file is read.
.submission_source("R/p4_helpers.R")

p4_oof_model_contracts <- function() {
  data.frame(
    model = c("Logistic Regression", "Random Forest", "XGBoost"),
    status = "pending", owner_threshold = NA_real_,
    oof_file = paste0("outputs/predictions/", c("logistic", "random_forest", "xgboost"),
                      "_oof_predictions.csv"),
    params_file = paste0("outputs/models/", c("logistic", "random_forest", "xgboost"),
                         "_params.rds"),
    n_training = NA_integer_, fold_source = "Pending OOF/parameter handoff.",
    stringsAsFactors = FALSE)
}

p4_oof_saved_threshold <- function(value, model) {
  grid <- seq(0.10, 0.90, by = 0.01)
  if (!is.numeric(value) || length(value) != 1L || !is.finite(value) ||
      min(abs(grid - value)) > 1e-8) {
    stop(model, ": saved owner threshold must lie on the 0.10--0.90, 0.01 grid.", call. = FALSE)
  }
  value
}

p4_oof_owner_threshold <- function(parameters, model) {
  field <- switch(model, "Logistic Regression" = "best_threshold",
                    "Random Forest" = "threshold", "XGBoost" = "best_threshold",
                    stop("Unknown OOF model: ", model, call. = FALSE))
  p4_oof_saved_threshold(parameters[[field]], model)
}

p4_oof_threshold_normalize <- function(data, train, saved_threshold, model,
                                        fold_id = NULL) {
  saved_threshold <- p4_oof_saved_threshold(saved_threshold, model)
  if (!is.data.frame(data) || anyDuplicated(names(data)) ||
      !all(c("actual", "probability_ad") %in% names(data))) {
    stop(model, ": OOF data require actual and probability_ad columns without duplicate names.", call. = FALSE)
  }
  aliases <- intersect(c("sample_id", "obs_id"), names(data))
  if (!length(aliases)) stop(model, ": OOF training IDs are missing.", call. = FALSE)
  if (length(aliases) == 2L &&
      !identical(as.character(data$sample_id), as.character(data$obs_id))) {
    stop(model, ": conflicting sample_id/obs_id aliases.", call. = FALSE)
  }
  ids <- data[[aliases[1L]]]
  if (anyNA(ids) || anyDuplicated(ids) || nrow(data) != nrow(train) ||
      !setequal(as.character(ids), as.character(train$obs_id))) {
    stop(model, ": OOF IDs must cover the exact shared training IDs once, without duplicates.", call. = FALSE)
  }
  data <- data[match(as.character(train$obs_id), as.character(ids)), , drop = FALSE]
  p4_validate_scores(data$actual, data$probability_ad)
  if (!identical(as.integer(data$actual), as.integer(train$y))) {
    stop(model, ": OOF actual labels disagree with shared training labels.", call. = FALSE)
  }
  predicted <- as.integer(data$probability_ad >= saved_threshold)
  if ("predicted" %in% names(data) &&
      (anyNA(data$predicted) || !all(data$predicted %in% 0:1) ||
       !identical(as.integer(data$predicted), predicted))) {
    stop(model, ": exported OOF labels disagree with the saved owner threshold.", call. = FALSE)
  }
  if (!is.null(fold_id) && "fold" %in% names(data) &&
      (anyNA(data$fold) || !identical(as.integer(data$fold), as.integer(fold_id)))) {
    stop(model, ": exported OOF fold labels disagree with the shared folds.", call. = FALSE)
  }
  out <- data.frame(sample_id = train$obs_id, actual = as.integer(data$actual),
                     predicted = predicted, probability_ad = data$probability_ad)
  if (!is.null(fold_id)) out$fold <- as.integer(fold_id)
  attr(out, "fold_source") <- if ("fold" %in% names(data)) {
    "Exported OOF fold labels verified against shared folds."
  } else {
    "Fold labels inferred by training ID from shared indices; not exported independently."
  }
  out
}

p4_oof_threshold_sweep <- function(data, saved_threshold, model) {
  saved_threshold <- p4_oof_saved_threshold(saved_threshold, model)
  grid <- seq(0.10, 0.90, by = 0.01)
  # Preserve the owner's exact floating-point cutoff at its matching grid point.
  grid[which.min(abs(grid - saved_threshold))] <- saved_threshold
  grid[which.min(abs(grid - 0.50))] <- 0.50
  do.call(rbind, lapply(grid, function(threshold) {
    m <- p4_metrics(data$actual, data$probability_ad, threshold = threshold)
    data.frame(model = model, threshold = threshold,
                 is_saved_threshold = abs(threshold - saved_threshold) < 1e-8,
                 as.list(m), row.names = NULL, check.names = FALSE)
  }))
}

p4_run_threshold_sensitivity <- function() {
  training_path <- "data/processed/train_raw.rds"
  folds_path <- "data/processed/cv_folds.rds"
  if (!all(file.exists(c(training_path, folds_path)))) {
    stop("Shared training data and CV folds are required.", call. = FALSE)
  }
  train <- readRDS(training_path)
  folds <- readRDS(folds_path)
  if (!all(c("obs_id", "y") %in% names(train)) || anyNA(train$obs_id) ||
      anyDuplicated(train$obs_id) || !identical(folds$obs_id, train$obs_id) ||
      !all(train$y %in% 0:1)) {
    stop("Shared training IDs, labels or fold alignment are invalid.", call. = FALSE)
  }
  fold_id <- integer(nrow(train))
  for (k in seq_len(folds$k)) fold_id[folds$indexOut[[k]]] <- k
  if (any(fold_id == 0L) || anyDuplicated(unlist(folds$indexOut)) ||
      !identical(sort(unname(unlist(folds$indexOut))), seq_len(nrow(train)))) {
    stop("Shared validation folds must cover training observations exactly once.", call. = FALSE)
  }
  models <- p4_oof_model_contracts()
  rf <- which(models$model == "Random Forest")
  if (!all(file.exists(c(models$oof_file[rf], models$params_file[rf])))) {
    stop("RF OOF predictions and saved parameters are required; run the RF workflow first.", call. = FALSE)
  }
  sweeps <- list()
  summaries <- list()
  used_inputs <- c(training_path, folds_path, "R/p4_helpers.R", "R/utils.R",
                    "R/preprocessing_pipeline.R", "R/14_p4_threshold_sensitivity.R")
  for (i in seq_len(nrow(models))) {
    paths <- c(models$oof_file[i], models$params_file[i])
    if (!all(file.exists(paths))) next
    parameters <- readRDS(models$params_file[i])
    cutoff <- p4_oof_owner_threshold(parameters, models$model[i])
    raw <- read.csv(models$oof_file[i], stringsAsFactors = FALSE, check.names = FALSE)
    normalized <- p4_oof_threshold_normalize(raw, train, cutoff, models$model[i], fold_id)
    models$status[i] <- "available"
    models$owner_threshold[i] <- cutoff
    models$n_training[i] <- nrow(normalized)
    models$fold_source[i] <- attr(normalized, "fold_source")
    used_inputs <- c(used_inputs, paths)
    sweeps[[i]] <- p4_oof_threshold_sweep(normalized, cutoff, models$model[i])
    summaries[[i]] <- do.call(rbind, lapply(c("saved", "reference_0.50"), function(role) {
      threshold <- if (role == "saved") cutoff else 0.50
      metrics <- p4_metrics(normalized$actual, normalized$probability_ad, threshold = threshold)
      data.frame(model = models$model[i], threshold_role = role, threshold = threshold,
                   n_training = nrow(normalized), as.list(metrics),
                   row.names = NULL, check.names = FALSE)
    }))
  }
  sweep <- do.call(rbind, sweeps)
  summary <- do.call(rbind, summaries)
  for (directory in c("outputs/models", "outputs/tables", "outputs/figures", "report/Part_4/figures")) {
    dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  }
  write.csv(sweep, "outputs/tables/comparison_oof_thresholds.csv", row.names = FALSE)
  write.csv(summary, "outputs/tables/comparison_oof_threshold_summary.csv", row.names = FALSE)
  available <- which(models$status == "available")
  three_panels <- length(available) == 3L
  p4_plot("fig_comparison_oof_thresholds", function() {
    old <- par(mfrow = if (three_panels) c(2, 2) else c(1, length(available)),
                 mar = c(4.5, 4.2, 5, 1),
                 oma = c(0, 0, 1.6, 0))
    on.exit(par(old), add = TRUE)
    colours <- c("#2a78d6", "#eb6834", "#5c8b45")
    for (i in available) {
      d <- sweep[sweep$model == models$model[i], , drop = FALSE]
      matplot(d$threshold, as.matrix(d[, c("Precision", "Recall", "F1")]),
                type = "l", col = colours, lty = 1, lwd = 2, ylim = c(0, 1),
                xlab = sprintf("Threshold (saved: %.2f)", models$owner_threshold[i]),
                ylab = "Training OOF score", main = "")
      abline(v = models$owner_threshold[i], col = "#444444", lty = 2)
      title(main = models$model[i], line = 2.3)
      legend("top", inset = c(0, -0.16), horiz = TRUE, xpd = NA,
               legend = c("Precision", "Recall", "F1"), col = colours,
               lty = 1, lwd = 2, bty = "n", cex = 0.80)
    }
    if (three_panels) {
      plot.new()
      text(0.5, 0.56, "Owner thresholds remain fixed.", cex = 0.95)
      text(0.5, 0.43, "Saved training OOF scores only;\nno test observations or model fitting.",
             cex = 0.88)
    }
    mtext("Training OOF threshold sensitivity", side = 3, outer = TRUE, line = 0.1, font = 2)
  }, width = if (three_panels) 9 else 4.5 * length(available),
     height = if (three_panels) 7.6 else 4.7)
  used_inputs <- .submission_input_paths(used_inputs)
  bundle <- list(
    status = if (all(models$status == "available")) "complete" else "pending",
    models = models, sweep = sweep, summary = summary,
    input_md5 = tools::md5sum(used_inputs),
    scope = paste("Descriptive saved-training-OOF threshold sensitivity only; owner thresholds stay fixed.",
                    "No test observations, model fitting, new threshold selection or multi-seed Logistic/XGBoost claim."))
  saveRDS(bundle, "outputs/models/p4_threshold_sensitivity.rds")
  message("[P4] Saved-OOF threshold sensitivity complete; no models trained or test files used.")
  invisible(bundle)
}

if (!isTRUE(getOption("p4.threshold.functions_only", FALSE))) p4_run_threshold_sensitivity()
})
# END EMBEDDED SOURCE: R/14_p4_threshold_sensitivity.R

# BEGIN EMBEDDED SOURCE: R/12_p4_report.R
.submission_modules[["R/12_p4_report.R"]] <- quote({
# =============================================================================
# 12_p4_report.R -- Generate P4's English report from reproducible outputs only.
# Execution: enabled by Rscript analysis.R --report after analysis is complete.
# No training, predictions, thresholds or other members' outputs are changed.
# =============================================================================
.submission_source("R/p4_helpers.R")
report_shared <- p4_init()
report_inputs <- c(
  "outputs/models/random_forest_params.rds", "outputs/models/p4_comparison.rds",
  "outputs/models/p4_robustness.rds", "outputs/tables/random_forest_performance.csv",
  "outputs/tables/random_forest_cv_grid.csv", "outputs/tables/random_forest_confusion_matrix.csv",
  "outputs/tables/random_forest_feature_importance.csv", "outputs/tables/random_forest_prediction_errors.csv"
)
if (!all(file.exists(report_inputs))) {
  stop("Run P4 scripts 08, 10 and 11 first. Missing: ",
       paste(report_inputs[!file.exists(report_inputs)], collapse = ", "), call. = FALSE)
}
report_params <- readRDS(report_inputs[1])
report_comparison <- readRDS(report_inputs[2])
report_robust <- readRDS(report_inputs[3])
report_performance <- read.csv(report_inputs[4], stringsAsFactors = FALSE)
report_grid <- read.csv(report_inputs[5], stringsAsFactors = FALSE)
report_cm <- read.csv(report_inputs[6], stringsAsFactors = FALSE)
report_importance <- read.csv(report_inputs[7], stringsAsFactors = FALSE)
report_errors <- read.csv(report_inputs[8], stringsAsFactors = FALSE)
report_selected <- report_grid[report_grid$selected, , drop = FALSE]
if (nrow(report_selected) != 1L) stop("Exactly one selected RF configuration is required.")
report_interpretation_path <- "outputs/models/p4_interpretation.rds"
report_interpretation <- if (file.exists(report_interpretation_path))
  readRDS(report_interpretation_path) else NULL
report_threshold_path <- "outputs/models/p4_threshold_sensitivity.rds"
report_threshold <- if (file.exists(report_threshold_path)) readRDS(report_threshold_path) else NULL
.submission_source("R/p4_artifact_checks.R")
report_checks <- p4_check_artifacts(report_shared, report_params, report_comparison,
                                    report_robust, report_interpretation, report_threshold)
p4_write_manifest(report_checks, "p4_artifact_checks")

rf_value <- function(metric, dataset = "Test") {
  answer <- report_performance[report_performance$Metric == metric, dataset]
  if (length(answer) != 1L || !is.finite(answer)) stop("Missing RF metric: ", metric)
  answer
}
f4 <- function(x) sprintf("%.4f", x)
f2 <- function(x) sprintf("%.2f", x)
p_tex <- function(x) ifelse(x < 0.0001, "<0.0001", paste0("=", f4(x)))
p_label <- function(x) ifelse(x < 0.0001, "<0.0001", f4(x))
pct <- function(x) paste0(sprintf("%.2f", 100 * x), "\\%")
esc <- p4_latex_escape
ci_text <- function(lo, hi) sprintf("[%.4f, %.4f]", lo, hi)
short_model <- function(x) {
  d <- c("Logistic Regression" = "Logistic", "Random Forest" = "RF", "XGBoost" = "XGBoost")
  unname(d[x])
}
report_join <- function(x) {
  if (length(x) < 2L) return(paste(x, collapse = ""))
  paste0(paste(head(x, -1L), collapse = ", "), " and ", tail(x, 1L))
}
write_section <- function(lines, name) {
  writeLines(paste(lines, collapse = "\n\n"),
             file.path("report/Part_4/sections", paste0(name, ".tex")), useBytes = TRUE)
}
figure <- function(name, caption, label, width = "0.90", placement = "htbp") {
  c(paste0("\\begin{figure}[", placement, "]"), "\\centering",
    paste0("\\includegraphics[width=", width, "\\textwidth]{figures/", name, ".pdf}"),
    paste0("\\caption{", caption, "}"), paste0("\\label{fig:", label, "}"), "\\end{figure}")
}
code_listing <- function(code, caption) {
  paste(c(paste0("\\begin{lstlisting}[language=R,frame=single,",
                 "basicstyle=\\ttfamily\\footnotesize,breaklines=true,",
                 "columns=fullflexible,keepspaces=true,showstringspaces=false,",
                 "caption={", caption, "}]"),
          code, "\\end{lstlisting}"), collapse = "\n")
}
code_excerpt <- function(path, first, after, caption, omit = NULL) {
  lines <- .submission_source_lines(path, warn = FALSE, encoding = "UTF-8")
  start <- which(lines == first)
  end <- which(lines == after)
  if (length(start) != 1L || length(end) != 1L || end <= start) {
    stop("Cannot locate a unique report code excerpt in ", path, call. = FALSE)
  }
  code <- lines[seq.int(start, end - 1L)]
  if (!is.null(omit)) code <- code[!grepl(omit, code)]
  code <- code[nzchar(trimws(code))]
  code_listing(code, caption)
}
keep_table_here <- function(name) {
  path <- file.path("report/Part_4/tables", paste0(name, ".tex"))
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  lines[1L] <- "\\begin{table}[H]"
  writeLines(lines, path, useBytes = TRUE)
}

# Compact report tables: keep the full machine-readable output separately.
report_parameter_table <- data.frame(
  Parameter = c("Trees", "Predictors considered per split", "Minimum node size",
                "OOF-selected threshold", "Training seed", "CV folds", "Predictors retained"),
  Value = c(report_params$ntree, report_params$effective_mtry, report_params$nodesize,
            report_params$threshold, report_params$seed, report_params$cv_folds,
            report_params$n_predictors)
)
p4_write_table(report_parameter_table, "report_rf_configuration", "Selected Random Forest configuration")
p4_write_table(report_performance[report_performance$Metric %in% c("Precision", "Recall", "F1", "PR_AUC"), ],
               "report_rf_performance", "Random Forest estimates by evaluation partition",
               "OOF scores informed configuration and threshold selection; they are descriptive and are not independent assessment scores.")
p4_write_table(report_cm, "report_rf_confusion", "Random Forest confusion matrix on the locked test set")

report_model_metrics <- report_comparison$metrics
report_comparison_table <- data.frame(
  Model = short_model(report_model_metrics$model),
  Accuracy = report_model_metrics$Accuracy, Precision = report_model_metrics$Precision,
  Recall = report_model_metrics$Recall, F1 = report_model_metrics$F1,
  ROC_AUC = report_model_metrics$ROC_AUC, PR_AUC = report_model_metrics$PR_AUC
)
p4_write_table(report_comparison_table, "report_model_metrics", "Consistently recomputed test metrics for available models",
               "Logistic = Logistic Regression; RF = Random Forest. All predictions are aligned to the same test IDs; ad is the positive class.")
keep_table_here("report_model_metrics")
report_ci <- report_comparison$intervals[
  report_comparison$intervals$metric %in% c("Recall", "F1", "PR_AUC"), , drop = FALSE]
report_ci_table <- data.frame(Model = short_model(report_ci$model),
                              Metric = gsub("_", "-", report_ci$metric),
                              Estimate = report_ci$estimate,
                              `95% CI` = ci_text(report_ci$lower, report_ci$upper),
                              check.names = FALSE)
p4_write_table(report_ci_table, "report_model_intervals", "Marginal percentile bootstrap intervals for key test metrics",
               paste(report_comparison$bootstrap_reps, "paired test-observation resamples. The fitted models and decision labels remain fixed."))

report_diff <- report_comparison$paired_differences[
  report_comparison$paired_differences$metric %in% c("Recall", "F1", "PR_AUC"), , drop = FALSE]
report_pr_pair <- report_diff[report_diff$metric == "PR_AUC", , drop = FALSE]
report_diff_table <- data.frame(
  Pair = paste(short_model(report_pr_pair$model_a), "minus", short_model(report_pr_pair$model_b)),
  Difference = report_pr_pair$estimate,
  `95% CI` = ci_text(report_pr_pair$lower, report_pr_pair$upper),
  `Holm p` = report_pr_pair$p_adjusted, check.names = FALSE
)
p4_write_table(report_diff_table, "report_paired_differences", "Paired PR-AUC differences between fitted models",
               "Differences use A minus B. Intervals are marginal 95% percentile intervals. Approximate centred-null bootstrap p-values are Holm-adjusted across PR-AUC model pairs.")
report_mc <- report_comparison$mcnemar
report_mc_table <- data.frame(
  Pair = paste(short_model(report_mc$model_a), "versus", short_model(report_mc$model_b)),
  `A correct / B wrong` = report_mc$a_correct_b_wrong,
  `A wrong / B correct` = report_mc$a_wrong_b_correct,
  `Exact p` = p_label(report_mc$p_value), `Holm p` = p_label(report_mc$p_adjusted), check.names = FALSE
)
p4_write_table(report_mc_table, "report_mcnemar", "Exact paired McNemar tests",
               "The test concerns discordant classification errors; it does not directly compare F1, ROC-AUC or PR-AUC.")
report_times <- data.frame(
  Model = short_model(report_model_metrics$model),
  `Final fit (s)` = report_model_metrics$Training_time_s,
  `Prediction (s)` = report_model_metrics$Prediction_time_s,
  `Tuning (s)` = report_model_metrics$Tuning_time_s, check.names = FALSE
)
p4_write_table(report_times, "report_model_times", "Exported computational times",
               "Missing values were not exported. Final fitting excludes preprocessing and tuning; RF includes permutation importance. Exported zero seconds reflect timer resolution. Timings are not a controlled hardware benchmark.")
report_errors_by_pair <- report_comparison$paired_errors
if (is.null(report_errors_by_pair)) stop("Rerun R/10_model_comparison.R to build paired error diagnostics.")
report_pair_table <- data.frame(
  Pair = paste(short_model(report_errors_by_pair$model_a), "vs", short_model(report_errors_by_pair$model_b)),
  Class = report_errors_by_pair$class,
  Both_correct = report_errors_by_pair$both_correct,
  Both_wrong = report_errors_by_pair$both_wrong,
  Only_A_correct = report_errors_by_pair$a_only_correct,
  Only_B_correct = report_errors_by_pair$b_only_correct
)
names(report_pair_table) <- c("Pair", "Class", "Both correct", "Both wrong",
                             "Only A correct", "Only B correct")
p4_write_table(report_pair_table, "report_paired_errors",
               "Paired classification outcomes by true class",
               "A and B follow the order in the Pair column. These counts describe the fixed exported labels; they are not used to retune a model.")
report_seed_table <- report_robust$seed_metrics[, c("seed", "Recall", "F1", "ROC_AUC", "PR_AUC")]
names(report_seed_table)[1] <- "Training seed"
p4_write_table(report_seed_table, "report_rf_seed_metrics", "RF pooled fixed-fold OOF performance across training seeds",
               "The configuration, folds, preprocessing rules and threshold remain fixed. Three seeds provide descriptive RNG sensitivity, not confidence intervals.")
report_threshold_models <- if (!is.null(report_threshold))
  report_threshold$models[report_threshold$models$status == "available", , drop = FALSE] else NULL
report_threshold_available <- !is.null(report_threshold_models) && nrow(report_threshold_models) > 0L
report_threshold_names <- if (report_threshold_available)
  report_join(esc(report_threshold_models$model)) else "Random Forest"
if (report_threshold_available) {
  s <- report_threshold$summary
  report_threshold_table <- data.frame(
    Model = short_model(s$model), Threshold = s$threshold,
    Role = ifelse(s$threshold_role == "saved", "Saved", "Reference"),
    Precision = s$Precision, Recall = s$Recall, F1 = s$F1)
  p4_write_table(report_threshold_table, "report_oof_threshold_summary",
                 "Training OOF metrics at saved and reference thresholds",
                 "Saved thresholds remain fixed for test evaluation. Reference 0.50 is descriptive; no threshold is reselected here.")
}

report_top <- head(report_importance, 3L)
report_test_counts <- c(ad = sum(report_shared$test$y == 1L),
                        nonad = sum(report_shared$test$y == 0L))
report_config_count <- nrow(report_grid)
report_seed_names <- paste(report_robust$seeds, collapse = ", ")
rf_tp <- report_cm$Predicted_ad[report_cm$Actual == "ad"]
rf_fn <- report_cm$Predicted_nonad[report_cm$Actual == "ad"]
rf_fp <- report_cm$Predicted_ad[report_cm$Actual == "nonad"]
rf_recall_ci <- report_ci[report_ci$model == "Random Forest" & report_ci$metric == "Recall", ]

write_section(c(
  "% Generated by R/12_p4_report.R from reproducible P4 outputs.",
  "\\subsection{Model 2 -- Random Forest}", "\\label{sec:p4_rf}",
  "\\subsubsection{Objective and Evaluation Design}",
  paste0("We assess whether Random Forest can identify advertisements using nonlinear combinations of the predictors~\\cite{breiman2001random,liaw2002classification}. The shared split contains $",
         nrow(report_shared$train), "$ training and $", nrow(report_shared$test),
         "$ test observations; advertisements account for ", pct(mean(report_shared$test$y)),
         " of the test sample. Recall measures the proportion of advertisements detected, while Precision and F1 describe the trade-off with false alarms. PR-AUC summarizes performance across thresholds."),
  paste0("Five-fold stratified cross-validation compares $", report_config_count,
         "$ configurations by mean fold PR-AUC. The selected forest has $", report_params$ntree,
         "$ trees, $mtry=", report_params$effective_mtry, "$ and minimum node size $",
         report_params$nodesize, "$. Preprocessing and class weights use each analysis fold only. Training out-of-fold (OOF) predictions select the fixed test threshold $\\tau=",
         f2(report_params$threshold),
         "$ by maximum F1. Because OOF scores inform both choices, they describe model development; the held-out test is used for final assessment."),
  code_excerpt("R/08_random_forest.R", "prep <- prep_fit(train, opts)",
               "oof_predicted <- as.integer(oof_probability >= threshold)",
               "RF fitting and test prediction after the configuration and threshold are fixed.",
               omit = "_start|_seconds|train_probability"),
  "\\subsubsection{Results and Statistical Interpretation}",
  "\\input{tables/report_rf_performance}",
  "\\input{tables/report_rf_confusion}",
  paste0("RF detects $", rf_tp, "/", report_test_counts[["ad"]], "$ advertisements (Recall ",
         f4(rf_value("Recall")), "), misses $", rf_fn, "$, and labels $", rf_fp, "/",
         report_test_counts[["nonad"]], "$ non-advertisements as advertisements. Test Precision is ",
         f4(rf_value("Precision")), " and F1 is ", f4(rf_value("F1")), "."),
  paste0("The bootstrap 95\\% interval for RF Recall is $",
         ci_text(rf_recall_ci$lower, rf_recall_ci$upper),
         "$ (Section~\\ref{sec:p4_comparison}); the limited number of test advertisements leaves appreciable uncertainty about the detection rate."),
  paste0("Training PR-AUC (", f4(rf_value("PR_AUC", "Train")), ") exceeds OOF (",
         f4(rf_value("PR_AUC", "OOF")), ") and test PR-AUC (", f4(rf_value("PR_AUC")),
         "). This training advantage indicates why fitted-sample accuracy alone is insufficient; the OOF--test gap does not by itself establish absence of overfitting."),
  figure("random_forest_feature_importance",
         "Leading RF predictors by training out-of-bag permutation importance.",
         "p4_rf_importance", "0.66"),
  paste0("The leading predictors are ", paste(paste0("\\texttt{", esc(report_top$feature), "}"), collapse = ", "),
         ". Permutation importance measures predictive contribution, rather than a causal effect; correlated indicators can share importance."),
  "\\paragraph{Conclusion.}",
  "RF provides a useful nonlinear comparator and detects most advertisements in this test sample. Its performance should be judged with the paired uncertainty analysis below, rather than from Accuracy or feature importance alone."
), "section5_3_random_forest")

report_missing_models <- report_comparison$availability$model[
  report_comparison$availability$status != "available"]
report_scope <- if (length(report_missing_models)) paste0(
  "The comparison currently includes ", report_join(esc(report_model_metrics$model)),
  "; outputs for ", report_join(esc(report_missing_models)),
  " remain pending, so a three-model conclusion is unavailable.") else
  "Logistic Regression, Random Forest and XGBoost are evaluated on exactly the same held-out observations."
report_best <- report_model_metrics$model[
  order(-report_model_metrics$PR_AUC, -report_model_metrics$Recall, -report_model_metrics$F1)][1]
report_pr_example <- if (nrow(report_pr_pair))
  report_pr_pair[which.max(abs(report_pr_pair$estimate)), , drop = FALSE] else NULL
report_pr_narrative <- if (!is.null(report_pr_example)) {
  r <- report_pr_example
  includes_zero <- r$lower <= 0 && r$upper >= 0
  paste0("For ", esc(r$model_a), " minus ", esc(r$model_b), ", the estimated PR-AUC difference is $",
    f4(r$estimate), "$ with marginal 95\\% CI $", ci_text(r$lower, r$upper),
    "$ and Holm-adjusted approximate bootstrap $p", p_tex(r$p_adjusted), "$. ",
    if (r$estimate < 0) paste0("The estimated difference favors ", esc(r$model_b), ". ") else
      if (r$estimate > 0) paste0("The estimated difference favors ", esc(r$model_a), ". ") else "",
    if (!includes_zero && r$p_adjusted > 0.05)
      "The marginal interval excludes zero, but the adjusted test does not reject at 5\\%. Marginal percentile intervals and multiplicity-adjusted tests need not give the same decision." else
      if (includes_zero) "The interval includes zero, leaving the direction of the difference uncertain." else
        "The marginal interval excludes zero; the adjusted test decision is reported separately.")
} else "At least two model outputs are needed for a paired difference."
report_pr_decision <- if (nrow(report_pr_pair) && all(report_pr_pair$p_adjusted > 0.05)) {
  "None of the PR-AUC comparisons rejects equal performance at the 5\\% level after Holm adjustment."
} else if (nrow(report_pr_pair)) paste0(
  "The adjusted PR-AUC comparison rejects at 5\\% for ",
  paste(paste(short_model(report_pr_pair$model_a[report_pr_pair$p_adjusted <= 0.05]),
              "versus", short_model(report_pr_pair$model_b[report_pr_pair$p_adjusted <= 0.05])),
        collapse = "; "), ".") else character()
report_mc_narrative <- if (nrow(report_mc) && all(report_mc$p_adjusted > 0.05)) {
  "None of the exact McNemar comparisons rejects equal discordant error probabilities at 5\\% after Holm adjustment. Failure to reject is not evidence that the models are equivalent."
} else if (nrow(report_mc)) paste0(
  "The exact McNemar comparison rejects at 5\\% after Holm adjustment for ",
  paste(paste(short_model(report_mc$model_a[report_mc$p_adjusted <= 0.05]), "versus",
              short_model(report_mc$model_b[report_mc$p_adjusted <= 0.05])), collapse = "; "),
  ". This concerns classification correctness, rather than F1 or PR-AUC.") else
  "At least two model outputs are needed for McNemar testing."
report_mc_example <- report_mc[report_mc$model_a == "Random Forest" &
                                report_mc$model_b == "XGBoost", , drop = FALSE]
report_mc_example_note <- if (nrow(report_mc_example)) paste0(
  "For RF versus XGBoost, RF alone is correct on $", report_mc_example$a_correct_b_wrong,
  "$ observations and XGBoost alone on $", report_mc_example$a_wrong_b_correct,
  "$. The apparent difference in correct classifications must therefore be assessed using these paired discordant counts.") else character()

write_section(c(
  "% Generated by R/12_p4_report.R from reproducible P4 outputs.",
  "\\subsection{Statistical Comparison of Predictive Performance}", "\\label{sec:p4_comparison}",
  "\\subsubsection{Statistical Objective and Paired Design}",
  report_scope,
  paste0("We estimate performance under the observation distribution represented by the $",
         nrow(report_shared$test), "$ test cases and assess whether observed differences exceed sampling uncertainty. PR-AUC receives emphasis because advertisements are the minority class (",
         report_test_counts[["ad"]], "/", nrow(report_shared$test),
         "); Recall and F1 provide complementary information. This emphasis is exploratory."),
  "All models use aligned test IDs, the same metric definitions and their development-selected thresholds. PR-AUC is recomputed by one grouped trapezoidal convention for every model; it is not average precision. The fitted models and thresholds remain fixed throughout inference.",
  "\\input{tables/report_model_metrics}",
  paste0(esc(report_best),
         " has the largest observed PR-AUC. This is a point estimate; statistical comparison requires the uncertainty of the paired differences."),
  "\\subsubsection{Bootstrap Estimation and Confidence Intervals}",
  paste0("A nonparametric paired bootstrap draws $B=", report_comparison$bootstrap_reps,
         "$ samples of $n=", nrow(report_shared$test),
         "$ test observations with replacement~\\cite{efron1993bootstrap}. The same sampled indices are applied to every model, preserving within-observation pairing. For each model pair we calculate"),
  "\\[\\widehat\\Delta_{A,B}=\\widehat{\\mathrm{PR\\!-\\!AUC}}_A-\\widehat{\\mathrm{PR\\!-\\!AUC}}_B,\\qquad CI_{95\\%}(\\Delta)=[q^*_{0.025},q^*_{0.975}],\\]",
  "where the quantiles come from the bootstrap distribution of the paired difference. The intervals describe test-sample uncertainty conditional on the fitted models; they exclude uncertainty from training, model selection and threshold selection.",
  "The following core loop is extracted from the submitted R helper; the model list and result arrays are initialized earlier.",
  code_excerpt("R/p4_helpers.R", "  set_project_seed(seed)",
               "  intervals <- do.call(rbind, lapply(models, function(m) {",
               "Paired resampling: all models receive the same sampled observations."),
  figure("fig_comparison_intervals",
         "Point estimates and marginal 95\\% bootstrap intervals. Paired differences, rather than overlap of individual intervals, determine the relative-performance assessment.",
         "p4_comparison_intervals", "0.92"),
  "\\input{tables/report_paired_differences}",
  report_pr_narrative,
  "Exploratory tests of $H_0:\\Delta_{A,B}=0$ versus $H_1:\\Delta_{A,B}\\ne0$ use approximate two-sided centred-null bootstrap p-values. Holm adjustment is applied across the PR-AUC model pairs; it is not a joint correction across every reported metric.",
  report_pr_decision,
  "\\subsubsection{Exact McNemar Test for Paired Classification Errors}",
  "McNemar's test asks whether the fixed classifiers differ in their probability of being correct on the same observation~\\cite{fagerland2013mcnemar}. Let $p_{10}=P(\\text{A correct, B wrong})$ and $p_{01}=P(\\text{A wrong, B correct})$. We test",
  "\\[H_0:p_{10}=p_{01}\\qquad\\text{versus}\\qquad H_1:p_{10}\\ne p_{01}.\\]",
  "Given $D=n_{10}+n_{01}$ discordant observations, $N_{10}\\mid D\\sim\\operatorname{Binomial}(D,0.5)$ under $H_0$. We use a two-sided exact binomial p-value and Holm adjustment across model pairs. The core calculation below uses an already aligned pair; the final line adjusts the collected pairwise p-values.",
  code_listing(c(
    "ca <- a$actual == a$predicted; cb <- b$actual == b$predicted",
    "n10 <- sum(ca & !cb); n01 <- sum(!ca & cb)",
    "D <- n10 + n01",
    "p_exact <- if (D == 0) 1 else binom.test(n10, D, p = 0.5)$p.value",
    "p_adjusted <- p.adjust(p_values, method = \"holm\")"),
    "Exact paired-error calculation and multiple-comparison adjustment."),
  "\\input{tables/report_mcnemar}",
  report_mc_example_note,
  report_mc_narrative,
  "McNemar assesses paired classification correctness; it does not directly test equality of F1 or PR-AUC.",
  "\\subsubsection{Statistical Conclusion and Scope}",
  paste0(esc(report_best),
         " is the leading empirical candidate in this experiment. The size and uncertainty of the paired differences, together with adjusted test results, govern how strongly this ordering can be interpreted. A larger point estimate alone does not establish superiority."),
  "Observation-level inference assumes sufficiently independent observation pairs. Images from the same website may be dependent, but website identifiers are unavailable; the results therefore do not establish website-level generalization. The exact conditional McNemar test can also be conservative."
), "section5_5_model_comparison")

report_ranges <- report_robust$seed_summary
seed_range <- function(metric) {
  r <- report_ranges[report_ranges$metric == metric, ]
  paste0("$", f4(r$minimum), "$--$", f4(r$maximum), "$")
}
report_rf_threshold_note <- if (report_threshold_available) {
  s <- report_threshold$summary
  saved <- s[s$model == "Random Forest" & s$threshold_role == "saved", , drop = FALSE]
  reference <- s[s$model == "Random Forest" & s$threshold_role == "reference_0.50", , drop = FALSE]
  if (nrow(saved) == 1L && nrow(reference) == 1L) paste0(
    "For RF, moving from the reference threshold $0.50$ to the saved $",
    f2(saved$threshold), "$ raises OOF Precision from ", f4(reference$Precision), " to ",
    f4(saved$Precision), " and lowers Recall from ", f4(reference$Recall), " to ",
    f4(saved$Recall), "; F1 changes from ", f4(reference$F1), " to ", f4(saved$F1),
    ". This quantifies the detection--false-alarm trade-off.") else character()
} else character()
write_section(c(
  "% Generated by R/12_p4_report.R from reproducible P4 outputs.",
  "\\subsection{Robustness and Sensitivity Analysis}", "\\label{sec:p4_robustness}",
  "\\subsubsection{Sensitivity to the RF Training Seed}",
  paste0("To assess numerical sensitivity to training randomness, RF is repeated with seeds $",
         report_seed_names, "$ while keeping the data, folds, preprocessing rules, configuration and threshold $",
         f2(report_params$threshold), "$ fixed. We compare pooled training OOF metrics; the test sample is not used."),
  "\\input{tables/report_rf_seed_metrics}",
  paste0("Across three seeds, OOF F1 ranges from ", seed_range("F1"), " and PR-AUC from ",
         seed_range("PR_AUC"),
         ". The narrow observed ranges describe limited sensitivity in these runs. They are not confidence intervals or evidence of robustness to new datasets; the best-performing seed is not substituted for the baseline."),
  "\\subsubsection{Sensitivity to the Classification Threshold}",
  if (report_threshold_available) c(
    paste0("Saved training OOF scores for ", report_threshold_names,
           " are evaluated over thresholds $0.10$--$0.90$. The table contrasts each saved threshold with $0.50$; all test thresholds remain fixed."),
    "\\input{tables/report_oof_threshold_summary}",
    report_rf_threshold_note
  ) else "Saved training OOF scores describe threshold sensitivity; the development-selected RF test threshold remains fixed.",
  "\\paragraph{Conclusion.}",
  paste0("The RF seed check describes training randomness, while threshold sensitivity quantifies a change in the decision rule. OOF scores helped select the configurations and thresholds, so these comparisons are descriptive development results, not independent validation. ",
    if (report_threshold_available && any(report_threshold_models$model != "Random Forest"))
      paste0("Multiple-seed evidence is available only for RF; no corresponding claim is made for ",
        report_join(esc(report_threshold_models$model[report_threshold_models$model != "Random Forest"])), ".") else "")
), "section5_6_robustness")

message("[P4] Generated concise Sections 5.3, 5.5 and 5.6 with statistical estimation and paired inference.")
})
# END EMBEDDED SOURCE: R/12_p4_report.R

  .submission_prepare_data <- function(validate_only = FALSE) {
    if (!file.exists(.submission_csv))
      stop("Put cleaned_data.csv beside analysis.R.", call. = FALSE)
    expected <- c("obs_id", "y", .submission_dictionary$column,
                  "set", "cv_fold", "split_order")
    classes <- c("integer", "integer", rep("numeric", nrow(.submission_dictionary)),
                 "character", "integer", "integer")
    data <- read.csv(.submission_csv, colClasses = classes, na.strings = "NA",
                     check.names = FALSE, stringsAsFactors = FALSE)
    if (!identical(names(data), expected) || nrow(data) != 2423L ||
        anyNA(data$obs_id) || anyDuplicated(data$obs_id) ||
        anyNA(data$y) || !all(data$y %in% 0:1) ||
        anyNA(data$set) || !all(data$set %in% c("train", "test")))
      stop("The cleaned CSV columns, IDs, labels or partitions are invalid.")
    metadata <- data[c("obs_id", "set", "cv_fold", "split_order")]
    clean <- data[order(data$obs_id), c("obs_id", "y", .submission_dictionary$column)]
    rownames(clean) <- NULL
    partition <- function(label) {
      m <- metadata[metadata$set == label, , drop = FALSE]
      if (anyNA(m$split_order) || anyDuplicated(m$split_order) ||
          !identical(sort(m$split_order), seq_len(nrow(m))))
        stop("Invalid within-partition order in cleaned_data.csv.")
      m <- m[order(m$split_order), , drop = FALSE]
      d <- clean[match(m$obs_id, clean$obs_id), , drop = FALSE]
      rownames(d) <- NULL
      list(data = d, metadata = m)
    }
    tr <- partition("train"); te <- partition("test")
    fold_id <- tr$metadata$cv_fold
    if (nrow(tr$data) != 1939L || nrow(te$data) != 484L ||
        anyNA(fold_id) || !all(fold_id %in% 1:5) || any(!is.na(te$metadata$cv_fold)) ||
        sum(tr$data$y) != 309L || sum(te$data$y) != 77L)
      stop("CSV train/test class counts or five-fold assignments are invalid.")
    folds <- list(k = 5L, seed = 42L, fold_id = fold_id, obs_id = tr$data$obs_id,
      index = lapply(1:5, function(k) which(fold_id != k)),
      indexOut = lapply(1:5, function(k) which(fold_id == k)))
    names(folds$index) <- names(folds$indexOut) <- sprintf("Fold%d", 1:5)
    if (isTRUE(validate_only)) {
      objects <- list(ad_clean = clean, train_raw = tr$data, test_raw = te$data, cv_folds = folds)
      for (n in names(objects)) {
        p <- paste0("data/processed/", n, ".rds")
        if (!file.exists(p) || !identical(objects[[n]], readRDS(p)))
          stop("Saved results do not match this cleaned CSV; run the full analysis first.")
      }
      p <- "data/processed/feature_dictionary.csv"
      if (!file.exists(p) || !identical(.submission_dictionary,
          read.csv(p, stringsAsFactors = FALSE, check.names = FALSE)))
        stop("Saved feature dictionary differs; run the full analysis first.")
      cat("Saved data verified against the supplied cleaned CSV; no inputs rewritten.\n")
      return(invisible(clean))
    }
    saveRDS(clean, "data/processed/ad_clean.rds")
    saveRDS(tr$data, "data/processed/train_raw.rds")
    saveRDS(te$data, "data/processed/test_raw.rds")
    saveRDS(folds, "data/processed/cv_folds.rds")
    write.csv(.submission_dictionary, "data/processed/feature_dictionary.csv", row.names = FALSE)
    write.csv(metadata, "data/processed/split_indices.csv", row.names = FALSE, na = "NA")
    cat("CSV verified: 2423 rows; train 1939 (309 ad), test 484 (77 ad); five shared folds.\n")
    invisible(clean)
  }
  .submission_run <- function(args) {
    known <- c("--check-data", "--report", "--from-raw", "--refresh")
    unknown <- setdiff(args, known)
    if (length(unknown)) stop("Unknown arguments: ", paste(unknown, collapse = ", "))
    if ("--from-raw" %in% args && any(c("--check-data", "--refresh") %in% args))
      stop("--from-raw is a full replay; use it separately from --check-data/--refresh.")
    previous <- getwd()
    on.exit(setwd(previous), add = TRUE)
    result_dir <- file.path(.submission_root, "results")
    dir.create(result_dir, recursive = TRUE, showWarnings = FALSE)
    setwd(result_dir)
    for (d in c("data/raw", "data/processed", "outputs/models", "outputs/predictions",
                "outputs/tables", "outputs/figures", "report/Part_4/sections",
                "report/Part_4/tables", "report/Part_4/figures"))
      dir.create(d, recursive = TRUE, showWarnings = FALSE)
    .submission_source("R/utils.R")
    .submission_source("R/preprocessing_pipeline.R")
    .submission_prepare_data(validate_only = "--refresh" %in% args)
    if ("--check-data" %in% args) return(invisible(result_dir))
    packages <- c("ggplot2", "glmnet", "randomForest", "xgboost", "PRROC", "Matrix")
    missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
    if (length(missing)) stop("Install required R packages before running: ",
                              paste(missing, collapse = ", "), call. = FALSE)
    if (as.character(utils::packageVersion("xgboost")) != "3.2.1.1")
      stop("This project's model checks require xgboost 3.2.1.1.", call. = FALSE)
    if (as.character(utils::packageVersion("PRROC")) != "1.4" ||
        as.character(utils::packageVersion("glmnet")) != "5.2")
      warning("Reference results use glmnet 5.2 and PRROC 1.4; installed versions differ.")
    old_workers <- Sys.getenv("P4_WORKERS", unset = NA_character_)
    old_bootstrap <- Sys.getenv("P4_BOOTSTRAP_REPS", unset = NA_character_)
    on.exit({
      if (is.na(old_workers)) Sys.unsetenv("P4_WORKERS") else Sys.setenv(P4_WORKERS = old_workers)
      if (is.na(old_bootstrap)) Sys.unsetenv("P4_BOOTSTRAP_REPS") else Sys.setenv(P4_BOOTSTRAP_REPS = old_bootstrap)
    }, add = TRUE)
    Sys.setenv(P4_WORKERS = "1", P4_BOOTSTRAP_REPS = "2000")
    stage <- function(file) {
      message("\n[Submission] Running ", file)
      .submission_source(file, local = new.env(parent = .submission_shared))
    }
    if ("--from-raw" %in% args) {
      raw <- file.path(.submission_root, c("ad.data", "ad.names"))
      if (!all(file.exists(raw))) stop("--from-raw requires ad.data and ad.names beside analysis.R.")
      stopifnot(all(file.copy(raw, file.path("data/raw", basename(raw)), overwrite = TRUE)))
      locked <- lapply(c("ad_clean", "train_raw", "test_raw", "cv_folds"),
                       function(n) readRDS(paste0("data/processed/", n, ".rds")))
      for (n in c("01_data_exploration", "02_data_preprocessing", "03_data_split"))
        stage(paste0("R/", n, ".R"))
      replayed <- lapply(c("ad_clean", "train_raw", "test_raw", "cv_folds"),
                         function(n) readRDS(paste0("data/processed/", n, ".rds")))
      if (!identical(locked, replayed))
        stop("Raw-data replay differs from the supplied cleaned CSV/split.")
    }
    protected <- c("data/processed/train_raw.rds", "data/processed/test_raw.rds",
                    "data/processed/cv_folds.rds", "data/processed/feature_dictionary.csv",
                    .submission_file, .submission_csv)
    hashes <- tools::md5sum(protected)
    write.csv(data.frame(file = names(hashes), md5 = unname(hashes)),
              "outputs/tables/p4_input_manifest.csv", row.names = FALSE)
    stages <- if ("--refresh" %in% args) c("10_model_comparison", "13_p4_interpretation",
                "14_p4_threshold_sensitivity") else
      c("04_descriptive_statistics", "05_estimation", "06_hypothesis_testing",
                "07_logistic_regression", "08_random_forest", "09_xgboost",
                "10_model_comparison", "11_robustness_analysis", "13_p4_interpretation",
                "14_p4_threshold_sensitivity")
    for (n in stages)
      stage(paste0("R/", n, ".R"))
    # Always validate cross-stage artifacts, including runs without LaTeX export.
    .submission_source("R/p4_helpers.R")
    .submission_source("R/p4_artifact_checks.R")
    checks <- with(.submission_shared, p4_check_artifacts(load_shared_data(),
      readRDS("outputs/models/random_forest_params.rds"),
      readRDS("outputs/models/p4_comparison.rds"),
      readRDS("outputs/models/p4_robustness.rds"),
      readRDS("outputs/models/p4_interpretation.rds"),
      readRDS("outputs/models/p4_threshold_sensitivity.rds")))
    write.csv(checks, "outputs/tables/p4_artifact_checks.csv", row.names = FALSE)
    stopifnot(identical(hashes, tools::md5sum(protected)))
    writeLines(capture.output(utils::sessionInfo()), "outputs/tables/submission_session_info.txt")
    if ("--report" %in% args) stage("R/12_p4_report.R")
    cat("\nAnalysis complete. Results:", normalizePath(getwd(), winslash = "/"), "\n")
    invisible(result_dir)
  }
  .submission_run(commandArgs(trailingOnly = TRUE))
})
