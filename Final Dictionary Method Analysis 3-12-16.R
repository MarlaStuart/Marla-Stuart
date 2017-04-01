## This is an analysis of journal article abstracts from social work journals
## from 1/1/2010 through 12/31/14
## Ctrl C or esc kills a process in the console -- escape, abort

## General Question: What is the state of social work research (quantity, reach, methods)?

## My approach:
## STEP 1. Automated text analysis using dictionary method (see code below).
## STEP 2. Create "key concepts" from all derivations of a word 

## Other ideas:
## Topic modeling with the classification and/or subject codes and/or words in each dictionary
## Split the list of abstracts in half. Do an analysis of the first half and consider
## that the prior in a Bayes analysis? Do an analysis of the second half and consider
## that the likelihood in a Bayes? 
## Some sort of fuzzy set analysis?
## I've tried topic modeling of the full abstract text, and also network analysis of 
## the terms found in the dictionary method. Neither seemed very useful.
## Could try topic modeling of the dictionary terms

##########
## Load data
require(tm)
require(RWeka)
setwd("C:/Users/Marla Stuart/Dropbox/Journal Articles Analysis/Data/FINAL Data")
corpus = Corpus(DirSource("C:/Users/Marla Stuart/Dropbox/Journal Articles Analysis/Data/FINAL Data"))

## Clean corpus
corpus <- tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removePunctuation)

## Check corpus
writeLines(as.character(corpus[400]))

## Create tokenize  function
yourTokenizer <- function(x) RWeka::NGramTokenizer(x, Weka_control(min = 1, max = 4))
#tokenized.corpus = yourTokenizer(corpus)

## Create dictionary
my.dictionary = c(
  "abduction",
  "abstract",
  "abstracted",
  "abstracting",
  "abstracts",
  "acs",
  "action research",
  "acyclic",
  "agency data",
  "agent based",
  "agent based simulation",
  "agentbased",
  "agentbased simulation",
  "american community survey",
  "analyses",
  "analysis",
  "analysis of variance",
  "analyze",
  "anonymity",
  "anova",
  "archival",
  "archive",
  "archived",
  "archiving",
  "artifact",
  "artifacts",
  "artificial intelligence based simulations",
  "assessment",
  "assessments",
  "associated",
  "associates",
  "association",
  "attitudes",
  "autobiography",
  "autoethnography",
  "baseline",
  "bayes",
  "bayesian",
  "bca",
  "benefit cost",
  "big data",
  "biographical",
  "biographical method",
  "biography",
  "bivariate",
  "box plot",
  "capi",
  "cartographic",
  "cartography",
  "case based",
  "case data",
  "case file",
  "case studies",
  "case study",
  "casebased",
  "casefile",
  "cataloging",
  "catalogue",
  "catalogues",
  "categorical",
  "causal",
  "causality",
  "causation",
  "cause",
  "caused",
  "causes",
  "causing",
  "cba",
  "census",
  "central tendency",
  "cfa",
  "chi",
  "chi square",
  "chisquare",
  "closed ended",
  "closedended",
  "cluster",
  "cluster analysis",
  "cluster sample",
  "cluster sampling",
  "clusteranalysis",
  "cochran",
  "code",
  "coded",
  "coding",
  "coefficient",
  "cohort",
  "cohorts",
  "community based research",
  "comparative",
  "comparative analysis",
  "compare",
  "compared",
  "comparing",
  "complex",
  "complexity",
  "computer assisted personal interviewing",
  "computer simulation",
  "confidential",
  "confidentiality",
  "confirmatory",
  "confirmatory factor analyses",
  "confirmatory factor analysis",
  "constructionism",
  "constructivism",
  "content analyses",
  "content analysis",
  "contingency table",
  "control",
  "control for",
  "controlled",
  "controlled for",
  "controlling for",
  "convenience sample",
  "convenience samples",
  "conversation analysis",
  "correlate",
  "correlated",
  "correlating",
  "correlation",
  "correlation matrix",
  "correlational",
  "cost benefit",
  "cost benefit analyses",
  "cost benefit analysis",
  "counterfactual",
  "critical",
  "critical realism",
  "critical theory",
  "cross tabulation",
  "crosssection",
  "crosssectional",
  "crosstab",
  "crosstabulation",
  "cultural analysis",
  "current study",
  "current analysis",
  "current sample",
  "dag",
  "data",
  "data archive",
  "data from",
  "data mine",
  "data mined",
  "data mining",
  "data quality",
  "database",
  "databases",
  "datum",
  "deduction",
  "deductive",
  "dependence",
  "describe",
  "described",
  "describes",
  "describing",
  "descriptive",
  "dialogic",
  "dialogical",
  "diary",
  "dictionary method",
  "difference in difference",
  "differenceindifference",
  "direct cost",
  "directive acyclic graph",
  "discount rate",
  "discourse analysis",
  "dispersion ",
  "distribution",
  "document",
  "document analyses",
  "document analysis",
  "document review",
  "document term matrix",
  "documenttermmatrix",
  "documentation",
  "documents review",
  "dynamic microsimulation",
  "dynamic microsimulations",
  "ecological fallacy",
  "effects",
  "eia",
  "electronic database",
  "electronic interview",
  "electronic questionnaire",
  "eligibility",
  "eligibility criteria",
  "eligible",
  "empirical",
  "empiricism",
  "environmental impact assessment",
  "epistemological",
  "epistemology",
  "eqia",
  "equality impact assessment ",
  "error",
  "errors",
  "ethno methodology",
  "ethnographic",
  "ethnography",
  "ethnomethodology",
  "euclidean",
  "evaluate",
  "evaluated",
  "evaluates",
  "evaluating",
  "evaluation",
  "evaluative",
  "evaluator",
  "event oriented observation design",
  "event oriented observation designs",
  "experiment",
  "experimented",
  "experimenter",
  "experimenting",
  "experiments",
  "explain",
  "explained",
  "explaining",
  "explains",
  "explanatory",
  "exploratory",
  "exploratory data analysis",
  "exploratory factor analysis",
  "explore",
  "explored",
  "explores",
  "exploring",
  "external validity",
  "factor analyses",
  "factor analysis",
  "falsifiable",
  "feminist empiricism",
  "feminist epistemology",
  "feminist postmodernism",
  "fidelity",
  "fidelity to the model",
  "with fidelity",
  "find",
  "finding",
  "findings",
  "fixed effect",
  "fixed effects",
  "focus group",
  "focus groups",
  "focused group",
  "focused groups",
  "four way interaction",
  "four way interactions",
  "fourway interaction",
  "fourway interactions",
  "framework",
  "frankfurt school",
  "fuzzy",
  "fuzzy set",
  "geographic information system",
  "geographic information systems",
  "gis",
  "grand theory",
  "gross effect",
  "gross effects",
  "grounded theory",
  "guttman",
  "guttman scale",
  "hawthorne effect",
  "health risk assessment ",
  "hermeneutic",
  "hermeneutics",
  "hierarchical",
  "hierarchical linear model",
  "hierarchical linear modeling",
  "hierarchical longitudinal model",
  "hierarchical longitudinal modeling",
  "higher",
  "historical",
  "historical analysis",
  "historical method",
  "historical methods",
  "history",
  "hlm",
  "hra",
  "hypotheses",
  "hypotheses testing",
  "hypothesis",
  "hypothesis testing",
  "ideal type",
  "impact assessment",
  "impact assessments",
  "independence",
  "independent",
  "indirect cost",
  "indirect costs",
  "induction",
  "inductive",
  "infer",
  "inference",
  "inferences",
  "inferred",
  "inferring",
  "infers",
  "informed consent",
  "instrument",
  "instrumentalvariable",
  "instrumentation",
  "instruments",
  "interaction",
  "interactions",
  "internal validity",
  "interpretive",
  "interpretive phenomenology",
  "interpretivism",
  "interpretivist",
  "interquartile range",
  "interval",
  "intervention research",
  "intervention study",
  "interview",
  "interviewed",
  "interviewer",
  "interviewer effect",
  "interviewer effects",
  "interviewing",
  "interviews",
  "item response",
  "kish select table",
  "latent",
  "latent class",
  "latent growth curve",
  "latent growth curves",
  "latent variable",
  "latent variable analyses",
  "latent variable analysis",
  "latent variables",
  "latitude",
  "least squares",
  "less likely",
  "life history",
  "likert scale",
  "linear",
  "listing method",
  "local average",
  "log",
  "logistic",
  "logistic regression",
  "logit",
  "log linear",
  "longitude",
  "longitudinal",
  "longitudinal design",
  "longitudinal designs",
  "longitudinal research",
  "lower",
  "map",
  "mapped",
  "mapping",
  "maps",
  "marginal effect",
  "marginal effects",
  "maturation effect",
  "maturation effects",
  "maximum likelihood estimate",
  "maximum likelihood estimates",
  "maximum likelihood estimation",
  "measure",
  "measured",
  "measurement",
  "measures",
  "measuring",
  "mediate",
  "mediated",
  "mediates",
  "mediating",
  "mediation",
  "meta",
  "meta analyses",
  "meta analysis",
  "metaanalyses",
  "metaanalysis",
  "method",
  "methodology",
  "middle range theory",
  "mixed method",
  "mixed methods",
  "mle",
  "model",
  "modeled",
  "modeling",
  "models",
  "moderate",
  "moderated",
  "moderates",
  "moderating",
  "moderation",
  "modernity",
  "more likely",
  "mortality effect",
  "mortality effects",
  "multi level",
  "multi method",
  "multilevel",
  "multilevel model",
  "multilevel models",
  "multimethod",
  "multiple regression",
  "multistage",
  "multi site",
  "multisite",
  "multivariate",
  "narrative",
  "narrative approach",
  "natural experiment",
  "natural experiments",
  "naturalism",
  "network",
  "network analyses",
  "network analysis",
  "neural network",
  "neural networks",
  "ngram",
  "ngrams",
  "nominal",
  "nonrandom",
  "nonrandomization",
  "nonrandomized",
  "normal distribution",
  "nway interactions",
  "observation",
  "observational",
  "observations",
  "observe",
  "observed",
  "observing",
  "ols",
  "online method",
  "online methods",
  "online survey",
  "online surveying",
  "online surveys",
  "open ended",
  "openended",
  "ordinal",
  "ordinary least squares",
  "outcome",
  "outcomes",
  "p",
  "paradigm",
  "paradigms",
  "participant",
  "participant observation",
  "participants",
  "participatory",
  "participatory action research",
  "participatory research",
  "pearson",
  "pearsons",
  "percent",
  "percents",
  "person based",
  "person centered",
  "personbased",
  "personcentered",
  "phenomenology",
  "phone survey",
  "phone surveys",
  "photo voice",
  "pilot",
  "piloted",
  "piloting",
  "pilot study",
  "policy analysis",
  "policy research",
  "positivism",
  "positivist",
  "post modernism",
  "post positivist",
  "post structuralism",
  "postmodern",
  "postmodernism",
  "postpositivist",
  "poststructuralism",
  "posttest",
  "posttreatment",
  "predict",
  "predicted",
  "predicting",
  "predictors",
  "present study",
  "present analysis",
  "present sample",
  "pretest",
  "pretesting",
  "pretreatment",
  "prevalence",
  "primary data",
  "principle component",
  "principle component analysis",
  "principle component analyses",
  "principle components",
  "principle components analysis",
  "principle components analyses",
  "probability",
  "propensity",
  "propensity score",
  "propensity scores",
  "prospective design",
  "psychometric",
  "qca",
  "qualitative",
  "qualitative comparative analyses",
  "qualitative comparative analysis",
  "qualitative generalization",
  "qualitative research",
  "quantitate",
  "quantitative",
  "quantitative generalization",
  "quartile",
  "quasi",
  "quasiexperiment",
  "quasiexperimental",
  "quasiexperiments",
  "queer research",
  "question order",
  "questionnaire",
  "questionnaires",
  "quota",
  "random",
  "random control trial",
  "random control trials",
  "random controlled trial",
  "random controlled trials",
  "random effect",
  "random effects",
  "random slope",
  "random slopes",
  "random walk",
  "randomization",
  "randomized",
  "randomized control trial",
  "randomized control trials",
  "randomness",
  "range",
  "rasch",
  "rate",
  "rated",
  "rating",
  "ratings",
  "ratio",
  "rct",
  "realism",
  "regression",
  "regression model",
  "regression models",
  "regression output",
  "regressions",
  "reliabilities",
  "reliability",
  "reliable",
  "research",
  "research design",
  "researcher",
  "response rate",
  "response rates",
  "result",
  "resulted",
  "results",
  "retroduction",
  "retrospective",
  "retrospective studies",
  "retrospective study",
  "sample",
  "sampled",
  "samples",
  "sampling",
  "scale",
  "scaled",
  "scales",
  "scaling",
  "secondary analyses",
  "secondary analysis",
  "secondary data",
  "secondary data analyses",
  "secondary data analysis",
  "self completed",
  "self completion",
  "selfcompletion",
  "selfcompleted",
  "sem",
  "semistructured",
  "semistructured interview",
  "semistructured interviews",
  "sia",
  "significant",
  "significantly",
  "simple random sample",
  "simulate",
  "simulated",
  "simulating",
  "simulation",
  "simultaneous equations",
  "simultaneous equation",
  "snowball",
  "social impact assessment ",
  "spatial",
  "spatial analyses",
  "spatial analysis",
  "spatial statistical analyses",
  "spatial statistical analysis",
  "srs",
  "standard deviation",
  "statistic",
  "statistical interaction",
  "statistical interactions",
  "statistical significance",
  "statistical testing",
  "statistically significant",
  "statistics",
  "stem and leaf",
  "stemandleaf",
  "stratified",
  "structural equation",
  "structural equation model",
  "structural equation modeling",
  "structural equation models",
  "structuralism",
  "structured interview",
  "structured interviews",
  "studied",
  "studies",
  "study",
  "subject",
  "subjects",
  "subscale",
  "subscales",
  "supervised learning",
  "survey",
  "surveyed",
  "surveying",
  "surveys",
  "symbolic interaction",
  "systematic review",
  "systematic reviews",
  "systematic sample",
  "telephone method",
  "telephone methods",
  "telephone survey",
  "telephone surveys",
  "termdocumentmatrix",
  "term document matrix",
  "test effect",
  "test effects",
  "testing effect",
  "testing effects",
  "text",
  "text analyses",
  "text analysis",
  "textual analyses",
  "textual analysis",
  "the sample",
  "the study",
  "the research",
  "the data",
  "the findings",
  "the analysis",
  "the current study",
  "thematic",
  "thematic analysis",
  "theme",
  "theoretical",
  "theories",
  "theorize",
  "theorized",
  "theory",
  "this sample",
  "this study",
  "this research",
  "this data",
  "this analysis",
  "these findings",
  "threeway interaction",
  "threeway interactions",
  "thurstone",
  "thurstone scale",
  "tiger file",
  "tiger files",
  "tigerfile",
  "tigerfiles",
  "tool",
  "tools",
  "transcribe",
  "transcription",
  "treatment effect",
  "treatment effects",
  "trial",
  "triangulate",
  "triangulation",
  "ttest",
  "two way interaction",
  "twostage",
  "twoway interactions",
  "type i",
  "type i error",
  "type i errors",
  "type ii",
  "type ii error",
  "type ii errors",
  "universalism",
  "unobtrusive measure",
  "unobtrusive measures",
  "unstructured interview",
  "unstructured interviews",
  "valid",
  "validate",
  "validates",
  "validities",
  "validity",
  "validity criteria",
  "variable",
  "variable based",
  "variablebased",
  "variables",
  "variance",
  "verbal protocol analysis",
  "verstehen",
  "vignette",
  "vignettes",
  "visual",
  "visual data",
  "visual method",
  "visual methods",
  "visual research method",
  "visual research methods",
  "voluntary consent",
  "vpa",
  "we studied",
  "we conducted",
  "we investigate",
  "we present",
  "web based survey",
  "web based surveys",
  "web survey",
  "web surveys",
  "webbased survey",
  "webbased surveys",
  "wilcoxon")

## Apply tokenizer and dictionary to corpus
corpus.dtm = DocumentTermMatrix(corpus, control = list(tokenize = yourTokenizer, dictionary = my.dictionary)) 

## Check corpus
dim(corpus.dtm)
class(corpus.dtm)

findFreqTerms(corpus.dtm, lowfreq=100) # have a look at common words
ncol(corpus.dtm) #number of unique words in corpus

## Find associations -- I think this is 2 words next to each other -- not associated in the
## same document
## Check the word frequencies to see which words I want to find associates for
## As is here, it is checking the association to ALL abstracts -- not to just those that are studies
findAssocs(corpus.dtm, "study", 0.1)
findAssocs(corpus.dtm, "data", 0.1)
findAssocs(corpus.dtm, "research", 0.1)
findAssocs(corpus.dtm, "database", 0.1)
findAssocs(corpus.dtm, "results", 0.1)
findAssocs(corpus.dtm, "abstract", 0.1)
findAssocs(corpus.dtm, "document", 0.1)
findAssocs(corpus.dtm, "this.study", 0.1)
findAssocs(corpus.dtm, "findings", 0.1)
findAssocs(corpus.dtm, "data", 0.1)
findAssocs(corpus.dtm, "analysis", 0.1)
findAssocs(corpus.dtm, "sample", 0.1)
findAssocs(corpus.dtm, "associated", 0.1)
findAssocs(corpus.dtm, "significant", 0.1)
findAssocs(corpus.dtm, "studies", 0.1)
findAssocs(corpus.dtm, "participants", 0.1)
findAssocs(corpus.dtm, "effects", 0.1)
findAssocs(corpus.dtm, "outcomes", 0.1)
findAssocs(corpus.dtm, "method", 0.1)
findAssocs(corpus.dtm, "model", 0.1)
findAssocs(corpus.dtm, "higher", 0.1)
findAssocs(corpus.dtm, "significantly", 0.1)
findAssocs(corpus.dtm, "survey", 0.1)
findAssocs(corpus.dtm, "measures", 0.1)
findAssocs(corpus.dtm, "the.study", 0.1)
findAssocs(corpus.dtm, "analyses", 0.1)
findAssocs(corpus.dtm, "compared", 0.1)
findAssocs(corpus.dtm, "theory", 0.1)
findAssocs(corpus.dtm, "interviews", 0.1)
findAssocs(corpus.dtm, "quantitative", 0.1)
findAssocs(corpus.dtm, "qualitative", 0.1)

# Convert dtm to a data frame (which needs to done to save as a CVS file and also do more analysis)
corpus.df = as.data.frame(inspect(corpus.dtm))
class(corpus.df)

# Remove spaces in column names
corpus.df = transform(corpus.df,dymmyvar=1)
corpus.df$dymmyvar = NULL
names(corpus.df)

# Create key concepts
corpus.df$Abstracts=corpus.df$abstract+corpus.df$abstracted+corpus.df$abstracting+corpus.df$abstracts
corpus.df$Analysis=corpus.df$analyses+corpus.df$analysis+corpus.df$analyze+corpus.df$current.analysis+corpus.df$present.analysis+corpus.df$the.analysis+corpus.df$this.analysis
corpus.df$ANOVA=corpus.df$analysis.of.variance+corpus.df$anova
corpus.df$Archival=corpus.df$archival+corpus.df$archive+corpus.df$archived+corpus.df$archiving+corpus.df$data.archive
corpus.df$Artifacts=corpus.df$artifact+corpus.df$artifacts
corpus.df$Bayesian=corpus.df$bayes+corpus.df$bayesian
corpus.df$Biographical=corpus.df$autobiography+corpus.df$biographical+corpus.df$biographical.method+corpus.df$biography
corpus.df$Case_based=corpus.df$case.based+corpus.df$case.data+corpus.df$case.file+corpus.df$case.studies+corpus.df$case.study+corpus.df$casebased+corpus.df$casefile
corpus.df$Cataloging=corpus.df$cataloging+corpus.df$catalogue+corpus.df$catalogues
corpus.df$Causality=corpus.df$causal+corpus.df$causality+corpus.df$causation+corpus.df$cause+corpus.df$caused+corpus.df$causes+corpus.df$causing+corpus.df$counterfactual+corpus.df$dependence+corpus.df$independence+corpus.df$independent+corpus.df$instrumentalvariable
corpus.df$Census=corpus.df$acs+corpus.df$american.community.survey+corpus.df$census
corpus.df$Chi_square=corpus.df$chi+corpus.df$chi.square+corpus.df$chisquare+corpus.df$contingency.table+corpus.df$cross.tabulation+corpus.df$crosstab+corpus.df$crosstabulation
corpus.df$Cluster_analysis=corpus.df$cluster+corpus.df$cluster.analysis+corpus.df$cluster.sample+corpus.df$cluster.sampling+corpus.df$clusteranalysis
corpus.df$Coding=corpus.df$code+corpus.df$coded+corpus.df$coding+corpus.df$deduction+corpus.df$deductive+corpus.df$induction+corpus.df$inductive
corpus.df$Comparative=corpus.df$comparative+corpus.df$comparative.analysis+corpus.df$compare+corpus.df$compared+corpus.df$comparing+corpus.df$higher+corpus.df$less.likely+corpus.df$lower+corpus.df$more.likely
corpus.df$Complexity=corpus.df$complex+corpus.df$complexity
corpus.df$Content_analysis=corpus.df$content.analyses+corpus.df$content.analysis+corpus.df$thematic+corpus.df$thematic.analysis+corpus.df$theme
corpus.df$Control=corpus.df$control+corpus.df$control.for+corpus.df$controlled+corpus.df$controlled.for+corpus.df$controlling.for
corpus.df$Correlation=corpus.df$associated+corpus.df$associates+corpus.df$association+corpus.df$coefficient+corpus.df$correlate+corpus.df$correlated+corpus.df$correlating+corpus.df$correlation+corpus.df$correlation.matrix+corpus.df$correlational+corpus.df$pearson+corpus.df$pearsons
corpus.df$Cost_benefit_analysis=corpus.df$bca+corpus.df$benefit.cost+corpus.df$cba+corpus.df$cost.benefit+corpus.df$cost.benefit.analyses+corpus.df$cost.benefit.analysis+corpus.df$direct.cost+corpus.df$discount.rate+corpus.df$indirect.cost+corpus.df$indirect.costs
corpus.df$Cultural_analysis=corpus.df$cultural.analysis
corpus.df$DAG=corpus.df$acyclic+corpus.df$dag+corpus.df$directive.acyclic.graph
corpus.df$Data=corpus.df$agency.data+corpus.df$big.data+corpus.df$data+corpus.df$datum+corpus.df$primary.data+corpus.df$data.from+corpus.df$the.data+corpus.df$this.data
corpus.df$Data_mining=corpus.df$data.mine+corpus.df$data.mined+corpus.df$data.mining
corpus.df$Database=corpus.df$database+corpus.df$databases+corpus.df$electronic.database
corpus.df$Descriptive=corpus.df$describe+corpus.df$described+corpus.df$describes+corpus.df$describing+corpus.df$descriptive
corpus.df$Discourse_analysis=corpus.df$conversation.analysis+corpus.df$dialogic+corpus.df$dialogical+corpus.df$discourse.analysis
corpus.df$Document_review=corpus.df$document+corpus.df$document.analyses+corpus.df$document.analysis+corpus.df$document.review+corpus.df$documentation+corpus.df$documents.review
corpus.df$Effects=corpus.df$effects+corpus.df$fixed.effect+corpus.df$fixed.effects+corpus.df$gross.effect+corpus.df$gross.effects+corpus.df$marginal.effect+corpus.df$marginal.effects+corpus.df$random.effect+corpus.df$random.effects+corpus.df$random.slope+corpus.df$random.slopes+corpus.df$test.effect+corpus.df$test.effects+corpus.df$testing.effect+corpus.df$testing.effects+corpus.df$treatment.effect+corpus.df$treatment.effects
corpus.df$Eligibility=corpus.df$eligibility+corpus.df$eligibility.criteria+corpus.df$eligible
corpus.df$Epistemology=corpus.df$constructionism+corpus.df$constructivism+corpus.df$critical+corpus.df$critical.realism+corpus.df$critical.theory+corpus.df$empirical+corpus.df$empiricism+corpus.df$epistemological+corpus.df$epistemology+corpus.df$feminist.empiricism+corpus.df$feminist.epistemology+corpus.df$feminist.postmodernism+corpus.df$frankfurt.school+corpus.df$interpretivist+corpus.df$interpretive+corpus.df$interpretive.phenomenology+corpus.df$interpretivism+corpus.df$modernity+corpus.df$phenomenology+corpus.df$positivism+corpus.df$positivist+corpus.df$post.modernism+corpus.df$post.positivist+corpus.df$post.structuralism+corpus.df$postmodern+corpus.df$postmodernism+corpus.df$postpositivist+corpus.df$poststructuralism+corpus.df$realism+corpus.df$structuralism+corpus.df$symbolic.interaction+corpus.df$universalism+corpus.df$naturalism+corpus.df$queer.research
corpus.df$Ethnographic=corpus.df$autoethnography+corpus.df$ethno.methodology+corpus.df$ethnographic+corpus.df$ethnography+corpus.df$ethnomethodology
corpus.df$Evaluation=corpus.df$evaluate+corpus.df$evaluated+corpus.df$evaluates+corpus.df$evaluating+corpus.df$evaluation+corpus.df$evaluative+corpus.df$evaluator
corpus.df$Experimental=corpus.df$baseline+corpus.df$experiment+corpus.df$experimented+corpus.df$experimenter+corpus.df$experimenting+corpus.df$experiments+corpus.df$local.average+corpus.df$natural.experiment+corpus.df$natural.experiments+corpus.df$posttest+corpus.df$posttreatment+corpus.df$pretest+corpus.df$pretesting+corpus.df$pretreatment+corpus.df$quasi+corpus.df$quasiexperiment+corpus.df$quasiexperimental+corpus.df$quasiexperiments+corpus.df$random.control.trial+corpus.df$random.control.trials+corpus.df$random.controlled.trial+corpus.df$random.controlled.trials+corpus.df$randomized.control.trial+corpus.df$randomized.control.trials+corpus.df$rct+corpus.df$trial
corpus.df$Explanatory=corpus.df$explain+corpus.df$explained+corpus.df$explaining+corpus.df$explains+corpus.df$explanatory
corpus.df$Exploratory=corpus.df$box.plot+corpus.df$exploratory+corpus.df$exploratory.data.analysis+corpus.df$explore+corpus.df$explored+corpus.df$explores+corpus.df$exploring+corpus.df$stem.and.leaf+corpus.df$stemandleaf
corpus.df$Factor_analysis=corpus.df$cfa+corpus.df$confirmatory+corpus.df$confirmatory.factor.analyses+corpus.df$confirmatory.factor.analysis+corpus.df$exploratory.factor.analysis+corpus.df$factor.analyses+corpus.df$factor.analysis
corpus.df$Fidelity=corpus.df$fidelity+corpus.df$fidelity.to.the.model+corpus.df$with.fidelity
corpus.df$Findings=corpus.df$find+corpus.df$finding+corpus.df$findings+corpus.df$the.findings+corpus.df$these.findings
corpus.df$Focus_groups=corpus.df$focus.group+corpus.df$focus.groups+corpus.df$focused.group+corpus.df$focused.groups
corpus.df$Fuzzy_set_analysis=corpus.df$fuzzy+corpus.df$fuzzy.set
corpus.df$Grounded_theory=corpus.df$grounded.theory
corpus.df$Hierarchical_methods=corpus.df$hierarchical+corpus.df$hierarchical.linear.model+corpus.df$hierarchical.linear.modeling+corpus.df$hierarchical.longitudinal.model+corpus.df$hierarchical.longitudinal.modeling+corpus.df$hlm+corpus.df$multi.level+corpus.df$multilevel+corpus.df$multilevel.model+corpus.df$multilevel.models
corpus.df$Historical_methods=corpus.df$historical+corpus.df$historical.analysis+corpus.df$historical.method+corpus.df$historical.methods+corpus.df$history
corpus.df$Hypotheses=corpus.df$hypotheses+corpus.df$hypotheses.testing+corpus.df$hypothesis+corpus.df$hypothesis.testing
corpus.df$Inference=corpus.df$abduction+corpus.df$ecological.fallacy+corpus.df$infer+corpus.df$inference+corpus.df$inferences+corpus.df$inferred+corpus.df$inferring+corpus.df$infers+corpus.df$retroduction
corpus.df$Informed_consent=corpus.df$anonymity+corpus.df$confidential+corpus.df$confidentiality+corpus.df$informed.consent+corpus.df$voluntary.consent
corpus.df$Interactions=corpus.df$four.way.interaction+corpus.df$four.way.interactions+corpus.df$fourway.interaction+corpus.df$fourway.interactions+corpus.df$interaction+corpus.df$interactions+corpus.df$nway.interactions+corpus.df$statistical.interaction+corpus.df$statistical.interactions+corpus.df$threeway.interaction+corpus.df$threeway.interactions+corpus.df$two.way.interaction+corpus.df$twoway.interactions
corpus.df$Intervention_research=corpus.df$intervention.research+corpus.df$intervention.study
corpus.df$Interviews=corpus.df$capi+corpus.df$computer.assisted.personal.interviewing+corpus.df$electronic.interview+corpus.df$interview+corpus.df$interviewed+corpus.df$interviewer.effect+corpus.df$interviewer.effects+corpus.df$interviewing+corpus.df$interviews+corpus.df$semistructured+corpus.df$semistructured.interview+corpus.df$semistructured.interviews+corpus.df$structured.interview+corpus.df$transcribe+corpus.df$transcription+corpus.df$unstructured.interview+corpus.df$unstructured.interviews+corpus.df$verbal.protocol.analysis+corpus.df$vpa+corpus.df$interviewer+corpus.df$structured.interviews
corpus.df$Latent_variables=corpus.df$latent+corpus.df$latent.class+corpus.df$latent.variable+corpus.df$latent.variable.analyses+corpus.df$latent.variable.analysis+corpus.df$latent.variables
corpus.df$Logistic_regression=corpus.df$log+corpus.df$logistic+corpus.df$logistic.regression+corpus.df$logit+corpus.df$log.linear
corpus.df$Longitudinal_methods=corpus.df$longitudinal+corpus.df$longitudinal.design+corpus.df$longitudinal.designs+corpus.df$longitudinal.research+corpus.df$prospective.design+corpus.df$retrospective+corpus.df$retrospective.studies+corpus.df$retrospective.study+corpus.df$latent.growth.curve+corpus.df$latent.growth.curves
corpus.df$Measurement=corpus.df$assessment+corpus.df$assessments+corpus.df$categorical+corpus.df$instrument+corpus.df$instrumentation+corpus.df$instruments+corpus.df$interval+corpus.df$measure+corpus.df$measured+corpus.df$measurement+corpus.df$measures+corpus.df$measuring+corpus.df$nominal+corpus.df$ordinal+corpus.df$ratio+corpus.df$unobtrusive.measure+corpus.df$unobtrusive.measures
corpus.df$Measurement_Error=corpus.df$error+corpus.df$errors+corpus.df$type.i+corpus.df$type.i.error+corpus.df$type.i.errors+corpus.df$type.ii+corpus.df$type.ii.error+corpus.df$type.ii.errors+corpus.df$psychometric
corpus.df$Mediation=corpus.df$mediate+corpus.df$mediated+corpus.df$mediates+corpus.df$mediating+corpus.df$mediation
corpus.df$Meta_analysis=corpus.df$meta+corpus.df$meta.analyses+corpus.df$meta.analysis+corpus.df$metaanalyses+corpus.df$metaanalysis
corpus.df$Miscellaneous_analysis_terms=corpus.df$central.tendency+corpus.df$dispersion.+corpus.df$distribution+corpus.df$interquartile.range+corpus.df$normal.distribution+corpus.df$percent+corpus.df$percents+corpus.df$prevalence+corpus.df$probability+corpus.df$quartile+corpus.df$range+corpus.df$standard.deviation+corpus.df$statistic+corpus.df$statistics+corpus.df$ttest+corpus.df$wilcoxon+corpus.df$variance+corpus.df$bivariate
corpus.df$Mixed_method=corpus.df$mixed.method+corpus.df$mixed.methods+corpus.df$multi.method+corpus.df$multimethod
corpus.df$Models=corpus.df$model+corpus.df$modeled+corpus.df$modeling+corpus.df$models
corpus.df$Moderation=corpus.df$moderate+corpus.df$moderated+corpus.df$moderates+corpus.df$moderating+corpus.df$moderation
corpus.df$Narrative_approaches=corpus.df$diary+corpus.df$life.history+corpus.df$narrative+corpus.df$narrative.approach+corpus.df$photo.voice+corpus.df$vignette+corpus.df$vignettes
corpus.df$Network_analysis=corpus.df$network+corpus.df$network.analyses+corpus.df$network.analysis+corpus.df$neural.network+corpus.df$neural.networks
corpus.df$Observational_methods=corpus.df$crosssection+corpus.df$crosssectional+corpus.df$event.oriented.observation.design+corpus.df$event.oriented.observation.designs+corpus.df$hawthorne.effect+corpus.df$observation+corpus.df$observational+corpus.df$observations+corpus.df$observe+corpus.df$observed+corpus.df$observing+corpus.df$participant.observation+corpus.df$difference.in.difference+corpus.df$differenceindifference
corpus.df$Online_survey=corpus.df$electronic.questionnaire+corpus.df$online.method+corpus.df$online.methods+corpus.df$online.survey+corpus.df$online.surveying+corpus.df$online.surveys+corpus.df$web.based.survey+corpus.df$web.based.surveys+corpus.df$web.survey+corpus.df$web.surveys+corpus.df$webbased.survey+corpus.df$webbased.surveys
corpus.df$Outcomes=corpus.df$eia+corpus.df$environmental.impact.assessment+corpus.df$eqia+corpus.df$equality.impact.assessment.+corpus.df$health.risk.assessment.+corpus.df$hra+corpus.df$impact.assessment+corpus.df$impact.assessments+corpus.df$multivariate+corpus.df$outcome+corpus.df$outcomes+corpus.df$result+corpus.df$resulted+corpus.df$results+corpus.df$sia+corpus.df$social.impact.assessment.
corpus.df$Participatory_methods=corpus.df$action.research+corpus.df$community.based.research+corpus.df$participatory+corpus.df$participatory.action.research+corpus.df$participatory.research+corpus.df$verstehen
corpus.df$Person_based=corpus.df$person.based+corpus.df$person.centered+corpus.df$personbased+corpus.df$personcentered
corpus.df$Pilot_test=corpus.df$pilot+corpus.df$piloted+corpus.df$piloting+corpus.df$pilot.study
corpus.df$Policy_analysis=corpus.df$policy.analysis+corpus.df$policy.research
corpus.df$Prediction=corpus.df$predict+corpus.df$predicted+corpus.df$predicting+corpus.df$predictors
corpus.df$Principal_component_analysis=corpus.df$principle.component+corpus.df$principle.components+corpus.df$principle.component.analysis+corpus.df$principle.component.analyses+corpus.df$principle.components.analysis+corpus.df$principle.components.analyses
corpus.df$Propensity_score=corpus.df$propensity+corpus.df$propensity.score+corpus.df$propensity.scores
corpus.df$QCA=corpus.df$qca+corpus.df$qualitative.comparative.analyses+corpus.df$qualitative.comparative.analysis
corpus.df$Qualitative=corpus.df$qualitative+corpus.df$qualitative.generalization+corpus.df$qualitative.research
corpus.df$Quantitative=corpus.df$quantitate+corpus.df$quantitative+corpus.df$quantitative.generalization
corpus.df$Rating=corpus.df$rate+corpus.df$rated+corpus.df$rating+corpus.df$ratings
corpus.df$Regression=corpus.df$least.squares+corpus.df$linear+corpus.df$maximum.likelihood.estimate+corpus.df$maximum.likelihood.estimation+corpus.df$mle+corpus.df$multiple.regression+corpus.df$ols+corpus.df$ordinary.least.squares+corpus.df$regression+corpus.df$regression.model+corpus.df$regression.models+corpus.df$regression.output+corpus.df$regressions+corpus.df$simultaneous.equations+corpus.df$maximum.likelihood.estimates+corpus.df$simultaneous.equation
corpus.df$Reliability=corpus.df$data.quality+corpus.df$reliabilities+corpus.df$reliability+corpus.df$reliable
corpus.df$Research=corpus.df$research+corpus.df$research.design+corpus.df$researcher+corpus.df$the.research+corpus.df$this.research
corpus.df$Response_rate=corpus.df$response.rate+corpus.df$response.rates
corpus.df$Sample=corpus.df$kish.select.table+corpus.df$listing.method+corpus.df$multistage+corpus.df$nonrandom+corpus.df$nonrandomization+corpus.df$nonrandomized+corpus.df$quota+corpus.df$random+corpus.df$random.walk+corpus.df$randomization+corpus.df$randomized+corpus.df$randomness+corpus.df$sample+corpus.df$sampled+corpus.df$samples+corpus.df$sampling+corpus.df$simple.random.sample+corpus.df$snowball+corpus.df$srs+corpus.df$stratified+corpus.df$systematic.sample+corpus.df$twostage+corpus.df$cohort+corpus.df$cohorts+corpus.df$convenience.sample+corpus.df$convenience.samples+corpus.df$current.sample+corpus.df$multi.site+corpus.df$multisite+corpus.df$present.sample+corpus.df$the.sample+corpus.df$this.sample
corpus.df$Scale=corpus.df$attitudes+corpus.df$guttman+corpus.df$guttman.scale+corpus.df$item.response+corpus.df$likert.scale+corpus.df$rasch+corpus.df$scale+corpus.df$scaled+corpus.df$scales+corpus.df$scaling+corpus.df$subscale+corpus.df$subscales+corpus.df$thurstone+corpus.df$thurstone.scale
corpus.df$Secondary_analysis=corpus.df$secondary.analyses+corpus.df$secondary.analysis+corpus.df$secondary.data+corpus.df$secondary.data.analyses+corpus.df$secondary.data.analysis
corpus.df$Self_completion=corpus.df$self.completed+corpus.df$self.completion+corpus.df$selfcompleted+corpus.df$selfcompletion
corpus.df$Simulation=corpus.df$agent.based+corpus.df$agent.based.simulation+corpus.df$agentbased+corpus.df$agentbased.simulation+corpus.df$artificial.intelligence.based.simulations+corpus.df$computer.simulation+corpus.df$dynamic.microsimulation+corpus.df$dynamic.microsimulations+corpus.df$simulate+corpus.df$simulated+corpus.df$simulating+corpus.df$simulation
corpus.df$Spatial_methods=corpus.df$cartographic+corpus.df$cartography+corpus.df$euclidean+corpus.df$geographic.information.system+corpus.df$geographic.information.systems+corpus.df$gis+corpus.df$latitude+corpus.df$longitude+corpus.df$map+corpus.df$mapped+corpus.df$mapping+corpus.df$maps+corpus.df$spatial+corpus.df$spatial.analyses+corpus.df$spatial.analysis+corpus.df$spatial.statistical.analyses+corpus.df$spatial.statistical.analysis+corpus.df$tiger.file+corpus.df$tigerfile+corpus.df$tigerfiles+corpus.df$tiger.files
corpus.df$Statistical_significance=corpus.df$p+corpus.df$significant+corpus.df$significantly+corpus.df$statistical.significance+corpus.df$statistical.testing+corpus.df$statistically.significant
corpus.df$Structural_equation_models=corpus.df$sem+corpus.df$structural.equation+corpus.df$structural.equation.model+corpus.df$structural.equation.modeling+corpus.df$structural.equation.models
corpus.df$Study=corpus.df$methodology+corpus.df$studied+corpus.df$studies+corpus.df$study+corpus.df$current.study+corpus.df$method+corpus.df$present.study+corpus.df$the.current.study+corpus.df$the.study+corpus.df$this.study+corpus.df$we.conducted+corpus.df$we.investigate+corpus.df$we.present+corpus.df$we.studied
corpus.df$Subjects=corpus.df$participant+corpus.df$participants+corpus.df$subject+corpus.df$subjects
corpus.df$Survey=corpus.df$closed.ended+corpus.df$open.ended+corpus.df$openended+corpus.df$question.order+corpus.df$questionnaire+corpus.df$questionnaires+corpus.df$survey+corpus.df$surveyed+corpus.df$surveying+corpus.df$surveys+corpus.df$closedended
corpus.df$Systematic_review=corpus.df$systematic.review+corpus.df$systematic.reviews+corpus.df$cochran
corpus.df$Telephone_survey=corpus.df$phone.survey+corpus.df$phone.surveys+corpus.df$telephone.method+corpus.df$telephone.methods+corpus.df$telephone.survey+corpus.df$telephone.surveys
corpus.df$Text_analysis=corpus.df$dictionary.method+corpus.df$documenttermmatrix+corpus.df$document.term.matrix+corpus.df$hermeneutic+corpus.df$hermeneutics+corpus.df$ngram+corpus.df$ngrams+corpus.df$supervised.learning+corpus.df$termdocumentmatrix+corpus.df$term.document.matrix+corpus.df$text+corpus.df$text.analyses+corpus.df$text.analysis+corpus.df$textual.analyses+corpus.df$textual.analysis
corpus.df$Theory=corpus.df$falsifiable+corpus.df$framework+corpus.df$grand.theory+corpus.df$ideal.type+corpus.df$middle.range.theory+corpus.df$paradigm+corpus.df$paradigms+corpus.df$theoretical+corpus.df$theories+corpus.df$theorize+corpus.df$theorized+corpus.df$theory
corpus.df$Tool=corpus.df$tool+corpus.df$tools
corpus.df$Validity=corpus.df$external.validity+corpus.df$internal.validity+corpus.df$maturation.effect+corpus.df$maturation.effects+corpus.df$mortality.effect+corpus.df$mortality.effects+corpus.df$triangulate+corpus.df$triangulation+corpus.df$valid+corpus.df$validate+corpus.df$validities+corpus.df$validity+corpus.df$validity.criteria+corpus.df$validates
corpus.df$Variable=corpus.df$variable+corpus.df$variable.based+corpus.df$variablebased+corpus.df$variables
corpus.df$Visual_methods=corpus.df$visual+corpus.df$visual.data+corpus.df$visual.method+corpus.df$visual.methods+corpus.df$visual.research.method+corpus.df$visual.research.methods

##Create Sub-dictionaries
corpus.df$Study_Concepts=corpus.df$Study+corpus.df$Research+corpus.df$Findings+corpus.df$Analysis+corpus.df$Data+
  corpus.df$Measurement+corpus.df$Variable

corpus.df$Design_Concepts=corpus.df$Comparative+corpus.df$Evaluation+corpus.df$Descriptive+corpus.df$Epistemology+corpus.df$Longitudinal_methods+
  corpus.df$Historical_methods+corpus.df$Explanatory+corpus.df$Complexity+corpus.df$Hypotheses+corpus.df$Validity+corpus.df$Reliability+
  corpus.df$Narrative_approaches+corpus.df$Pilot_test+corpus.df$Ethnographic+corpus.df$Eligibility+corpus.df$Propensity_score+corpus.df$Inference+
  corpus.df$Informed_consent+corpus.df$Intervention_research+corpus.df$Biographical+corpus.df$Experimentalcorpus.df$Exploratory+corpus.df$Fidelity+
  corpus.df$Grounded_theory+corpus.df$Mixed_method+corpus.df$Outcomes+corpus.df$Participatory_methods_corpus.df$Theory

corpus.df$Data_Concepts=corpus.df$Document_review+corpus.df$Subjects+corpus.df$Survey+corpus.df$Scale+corpus.df$Rating+
  corpus.df$Tool+corpus.df$Case_based+corpus.df$Focus_groups+corpus.df$Response_rate+corpus.df$Archival+corpus.df$Telephone_survey+corpus.df$Artifacts+
  corpus.df$Self_completion+corpus.df$Artifacts+corpus.df$Census+corpus.df$Interviews+corpus.df$Measurement_Error+corpus.df$Observational_methods+
  corpus.df$Online_survey+corpus.df$Sample_corpus.df$Cataloging

corpus.df$Analysis_Concepts=corpus.df$Correlation+corpus.df$Statistical_significance+corpus.df$Models+corpus.df$Miscellaneous_analysis_terms+
  corpus.df$Effects+corpus.df$Control+corpus.df$Prediction+corpus.df$Causality+corpus.df$Qualitative+corpus.df$Moderation+corpus.df$Mediation+
  corpus.df$Logistic_regression+corpus.df$Network_analysis+corpus.df$Content_analysis+corpus.df$Quantitative+corpus.df$Coding+corpus.df$Meta_analysis+
  corpus.df$Systematic_review+corpus.df$Cluster_analysis+corpus.df$ANOVA+corpus.df$Bayesian+corpus.df$Chi_square+corpus.df$Cost_benefit_analysis+
  corpus.df$Cultural_analysis+corpus.df$Data_mining+corpus.df$Discourse_analysis+corpus.df$DAG+corpus.df$Factor_analysis+corpus.df$Fuzzy_set_analysis+
  corpus.df$Hierarchical_methods+corpus.df$Interactions+corpus.df$Latent_variables+corpus.df$Person_basedcorpus.df$Policy_analysis+
  corpus.df$Principal_component_analysis+corpus.df$QCA+corpus.df$Regression+corpus.df$Secondary_analysis+corpus.df$Simulation+corpus.df$Spatial_methods+
  corpus.df$Structural_equation_models+corpus.df$Text_analysis+corpus.df$Visual_methods

# Export as .csv
write.csv(corpus.df, file = "my.dictionaryFINAL.csv") 

