// set up variables describing this specific task version

// task version
var dbCollection = "causal-train-mh";  // name of the data collection in firestore
var redirect = "https://app.prolific.com/submissions/complete?cc=YOUR_COMPLETION_CODE";  // where to redirect participants after the task is completed
var debugging = false;				   // !!set to "false" for real exp!

// task info
var infoSheet = "./assets/participant-information-sheet-v4_140725.pdf";

// uncomment the task version you want to run, and comment out the others ----------------------------------------------
// prescreen
// var version = "prescreen"; // name of this task version
// var hourlyRate = 7.5;							// 7.50 hourly rate (GBP)
// var briefStudyDescr = "In this study, we will ask you to provide some information about yourself, including your mood and emotions.";
// var taskQuestions = true;
// var timepoint = 1;
// var intCond = null; // ignored
// var consentNeeded = true;
// var approxTime = 8;
// var session = 1; // ignored

// causal learning task - ctr websites
// var version = "training-causal";
// var hourlyRate = 7.5;							// 7.50 hourly rate (GBP)
// var briefStudyDescr = "In this study, we will ask you to learn about the causes of different events.";
// var taskQuestions = false;
// var timepoint = null; // ignored
// var intCond = "causal";
// var consentNeeded = false;
// var approxTime = 12;
// var session = 6; // training session number (1 to 6)

// control learning task - ltr websites
// var version = "training-control";
// var hourlyRate = 7.5;							// 7.50 hourly rate (GBP)
// var briefStudyDescr = "In this study, we will ask you to learn about different objects and make choices about them.";
// var taskQuestions = false;
// var intCond = "control";
// var timepoint = null; // ignored
// var consentNeeded = false;
// var approxTime = 10;
// var session = 6; // training session number (1 to 6)

// final questionnaires/task
var version = "postscreen";
var hourlyRate = 7.5;							// 7.50 hourly rate (GBP)
var briefStudyDescr = "Thank you so much for your participation over the past two weeks. In this final part of the study, we will ask you to complete some questionnaires and a task.";
var taskQuestions = true;
var timepoint = 2;
var intCond = "control"; // make separate versions for each condition
// var intCond = "causal"; // uncomment to run causal condition
var consentNeeded = false;
var approxTime = 20;
var session = 1; // ignored

// ---------------------------------------------------------------------------------------------------------------------

// time and payment info for this task version

var baseEarn = ((approxTime/60)*hourlyRate);   	// base payment for this task version (GBP)
var nQuests = 5;  								// how many questionnaires will we ask participant to complete?      
let allowDevices = false;                		// allow participants to access this task on mobile devices?

// set task variables
var nBlocksChoice = 2;
var nBlocksLearning = 3;
var nScenarios = "three";

export { 
	dbCollection, version, redirect, infoSheet, briefStudyDescr, intCond, taskQuestions,
	debugging, timepoint, approxTime, hourlyRate, baseEarn, nQuests, allowDevices,
	nBlocksChoice, nBlocksLearning, nScenarios, consentNeeded, session
};