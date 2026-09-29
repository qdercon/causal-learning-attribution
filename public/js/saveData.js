// Helper functions for saving trial data using firebase's firestore

// import task version info
import { dbCollection, version, intCond, session } from "./versionInfo.js";

// import jspsych object so can access modules
import { jsPsych } from "./constructStudy.js";

// enable persistence 
firebase.firestore().enablePersistence()
    .catch(function(err) {
      if (err.code == 'failed-precondition') {  // multiple tabs open, persistence can only be enabled in one tab at a a time
      } else if (err.code == 'unimplemented') { // the current browser does not support all of the features required to enable persistence
      }
    });

// initialize db
var db = firebase.firestore();

// function to save consent 
var saveConsent = function() {
  db.collection('tasks').doc(dbCollection).collection(version).doc(uid).set({
    firebaseUID: uid,                     // firebase user ID, see firebaseAuth.js
    prolificSubID: subjectID,             // prolific subject ID, see getProlificID.js
    prolificStudyID: studyID,             // prolific study ID, see getProlificID.js
    consentObtained: 'yes',               // participant only proceeds to this point if they agree to all consent items
    interventionCondition: "ineligible",  // intervention condition
    consentDate: new Date().toISOString().split('T')[0],
    consentTime: new Date().toLocaleTimeString(),
    participantOS: navigator.userAgent
  }); 
};

// function to save initial data for tasks/questionnaires post-initial-consent
var saveInitData = function() {
  return db.collection('tasks').doc(dbCollection).collection(version).doc(uid).set({
      firebaseUID: uid,                     // firebase user ID, see firebaseAuth.js
      prolificSubID: subjectID,             // prolific subject ID, see getProlificID.js
      prolificStudyID: studyID,             // prolific study ID, see getProlificID.js
      interventionCondition: intCond,       // intervention condition
      sessionNo: session,                // session number (1 to 6 if one of the training tasks)
      startDate: new Date().toISOString().split('T')[0],
      startTime: new Date().toLocaleTimeString(),
      participantOS: navigator.userAgent
    }); 
};

// function to save initial data
var saveStartData = function(startTime){
  db.collection('tasks').doc(dbCollection).collection(version).doc(uid).update({
    taskStartTimeJsPsych: startTime,
    expCompleted: 0
  });
  // initialize data-storage collections
  db.collection('tasks').doc(dbCollection).collection(version).doc(uid).collection('task-data').doc('data').set({init: 1});
  db.collection('tasks').doc(dbCollection).collection(version).doc(uid).collection('quest-data').doc('data').set({init: 1});
  // db.collection('tasks').doc(dbCollection).collection(version).doc(uid).collection('task-data').doc('data-backup').set({init: 1});
};

// function to save the main task data
var saveTaskData = function(trialN, dataToSave) {
  db.collection('tasks').doc(dbCollection).collection(version).doc(uid).collection('task-data').doc('data').update({[trialN]: dataToSave});
  db.collection('tasks').doc(dbCollection).collection(version).doc(uid).update({
      totalTimeJsPsych: jsPsych.getTotalTime()
  });
};

// function to save questionnaire data
var saveQuestData = function (questionnaire, dataToSave, completionRT) {
  db.collection('tasks').doc(dbCollection).collection(version).doc(uid).collection('quest-data').doc('data').update({
    [questionnaire]: dataToSave,
    [questionnaire+'_RT']: completionRT
  });
};

var saveEligData = function(is_eligible, reason) {
  // save eligibility info
  db.collection('tasks').doc(dbCollection).collection(version).doc(uid).update({
    eligibility: is_eligible,
    reasonIneligible: reason
  });
}

var saveCondData = function(assignment) {
  // save group assignment info
  db.collection('tasks').doc(dbCollection).collection(version).doc(uid).update({
    interventionCondition: assignment
  });
}

// function to save end data 
// ...existing code...
// function to save end data 
var saveEndData = function(){
  // save end time info
  return db.collection('tasks').doc(dbCollection).collection(version).doc(uid).update({
    expEndTime: new Date().toLocaleTimeString(),
    expCompleted: 1
  });
  // // data-dump in case of any issues
  // var dataBackup = jsPsych.data.get();
  // db.collection('tasks').doc(dbCollection).collection(version).doc(uid).collection('task-data').doc('data-backup').update(dataBackup);
};

export { saveConsent, saveInitData, saveStartData, saveTaskData, saveQuestData, saveEligData, saveCondData, saveEndData }
