var jsPsychHtmlMultiSliderResponse = (function (jspsych) {
  'use strict';

  var version = "2.1.0";

  const info = {
    name: "html-slider-response",
    version,
    parameters: {
      /** The HTML string to be displayed */
      stimulus: {
        type: jspsych.ParameterType.HTML_STRING,
        default: void 0
      },
      /** Sets the minimum value of the slider. */
      min: {
        type: jspsych.ParameterType.INT,
        default: 0
      },
      /** Sets the maximum value of the slider */
      max: {
        type: jspsych.ParameterType.INT,
        default: 100
      },
      /** Sets the starting value of the slider */
      slider_start: {
        type: jspsych.ParameterType.INT, // If multiple sliders have different starts, this would need to be an array. Current plugin assumes one start value for all.
        default: 50
      },
      /** Sets the step of the slider. This is the smallest amount by which the slider can change. */
      step: {
        type: jspsych.ParameterType.INT,
        default: 1
      },
      /** 
       * Labels displayed at equidistant locations on the slider. Can only handle two labels for a single slider.
       * For side labels: 
       * - If one slider: `labels: ['Left Label', 'Right Label']`
       * - If multiple sliders (`multiple_sliders: true`): `labels: [['Slider1 Left', 'Slider1 Right'], ['Slider2 Left', 'Slider2 Right']]`
       */
      labels: {
        type: jspsych.ParameterType.COMPLEX, // Changed from HTML_STRING to COMPLEX to better support array of arrays for multiple_sliders
        default: [],
        array: true // Indicates that the base type is an array.
      },
      /** Are there multiple sliders? If so, they should be specified as an array of arrays. */
      multiple_sliders: {
        type: jspsych.ParameterType.BOOL,
        default: false
      },
      /** Set the width of the slider in pixels. If left null, then the width will be equal to the widest element in the display. */
      slider_width: {
        type: jspsych.ParameterType.INT,
        default: null
      },
      /** Label of the button to end the trial. */
      button_label: {
        type: jspsych.ParameterType.STRING,
        default: "Continue",
        array: false
      },
      /** If true, the participant must move the slider before clicking the continue button. */
      require_movement: {
        type: jspsych.ParameterType.BOOL,
        default: false
      },
      /** This string can contain HTML markup. Any content here will be displayed below the stimulus. The intention is that it can be used to provide a reminder about the action the participant is supposed to take (e.g., which key to press). */
      prompt: {
        type: jspsych.ParameterType.HTML_STRING,
        default: null
      },
      /** How long to display the stimulus in milliseconds. The visibility CSS property of the stimulus will be set to `hidden` after this time has elapsed. If this is null, then the stimulus will remain visible until the trial ends. */
      stimulus_duration: {
        type: jspsych.ParameterType.INT,
        default: null
      },
      /** How long to wait for the participant to make a response before ending the trial in milliseconds. If the participant fails to make a response before this timer is reached, the participant's response will be recorded as null for the trial and the trial will end. If the value of this parameter is null, then the trial will wait for a response indefinitely. */
      trial_duration: {
        type: jspsych.ParameterType.INT,
        default: null
      },
      /** If true, then the trial will end whenever the participant makes a response (assuming they make their response before the cutoff specified by the `trial_duration` parameter). If false, then the trial will continue until the value for `trial_duration` is reached. You can set this parameter to `false` to force the participant to view a stimulus for a fixed amount of time, even if they respond before the time is complete. */
      response_ends_trial: {
        type: jspsych.ParameterType.BOOL,
        default: true
      }
    },
    data: {
      /** The time in milliseconds for the participant to make a response. The time is measured from when the stimulus first appears on the screen until the participant's response. */
      rt: {
        type: jspsych.ParameterType.INT
      },
      /** 
       * The numeric value(s) of the slider(s). 
       * If one slider, this is an INT. 
       * If multiple sliders, this is an ARRAY of INTs.
       */
      response: {
        type: jspsych.ParameterType.COMPLEX, // Changed to COMPLEX to allow for INT or ARRAY of INTs
        default: undefined 
      },
      /** The HTML content that was displayed on the screen. */
      stimulus: {
        type: jspsych.ParameterType.HTML_STRING
      },
      /** The starting value of the slider. */
      slider_start: {
        type: jspsych.ParameterType.INT // Remains INT as per parameter definition
      }
    },
    // prettier-ignore
    citations: {
      "apa": "de Leeuw, J. R., Gilbert, R. A., & Luchterhandt, B. (2023). jsPsych: Enabling an Open-Source Collaborative Ecosystem of Behavioral Experiments. Journal of Open Source Software, 8(85), 5351. https://doi.org/10.21105/joss.05351 ",
      "bibtex": '@article{Leeuw2023jsPsych, 	author = {de Leeuw, Joshua R. and Gilbert, Rebecca A. and Luchterhandt, Bj{\\" o}rn}, 	journal = {Journal of Open Source Software}, 	doi = {10.21105/joss.05351}, 	issn = {2475-9066}, 	number = {85}, 	year = {2023}, 	month = {may 11}, 	pages = {5351}, 	publisher = {Open Journals}, 	title = {jsPsych: Enabling an {Open}-{Source} {Collaborative} {Ecosystem} of {Behavioral} {Experiments}}, 	url = {https://joss.theoj.org/papers/10.21105/joss.05351}, 	volume = {8}, }  '
    }
  };

  class HtmlSliderResponsePlugin {
    constructor(jsPsych) {
      this.jsPsych = jsPsych;
    }
    static {
      this.info = info;
    }
    trial(display_element, trial) {
      var html = '<div id="jspsych-html-slider-response-wrapper"';
      html += '<div id="jspsych-html-slider-response-stimulus">' + trial.stimulus + "</div>";

      let num_sliders_to_render;
      let slider_configs = []; 

      if (trial.multiple_sliders) {
        if (Array.isArray(trial.labels) && trial.labels.length > 0 && trial.labels.every(l => Array.isArray(l))) {
          num_sliders_to_render = trial.labels.length;
          for (let i = 0; i < num_sliders_to_render; i++) {
            slider_configs.push({ labels: trial.labels[i], start: trial.slider_start });
          }
        } else {
          num_sliders_to_render = 0;
          console.warn("jsPsychHtmlMultiSliderResponse: 'multiple_sliders' is true, but 'labels' is not an array of label-arrays (e.g., [['L1','R1'], ['L2','R2']]). No sliders will be rendered.");
        }
      } else {
        num_sliders_to_render = 1;
        slider_configs.push({ labels: trial.labels || [], start: trial.slider_start });
      }

      for (var i = 0; i < num_sliders_to_render; i++) {
        const current_config = slider_configs[i];
        const label_set_for_slider_i = current_config.labels;
        const start_value_for_slider_i = current_config.start;

        html += `<div class="jspsych-html-slider-response-container" style="position:relative; margin: 0 auto 3em auto; display: flex; flex-direction: column; align-items: center; ${trial.slider_width !== null ? `width:${trial.slider_width}px;` : 'width:auto; max-width:90%;'}">`;
        html += '<div style="display: flex; align-items: center; width: 100%;">';

        if (label_set_for_slider_i.length > 0 && label_set_for_slider_i[0] !== null && typeof label_set_for_slider_i[0] !== 'undefined') {
          html += `<span style="margin-right: 10px; font-size:0.8em; white-space:normal; word-break: break-word; text-align: center; flex-basis: 25%;; flex-shrink: 1;">${label_set_for_slider_i[0]}</span>`;
        } else {
          html += `<span style="margin-right: 10px; flex-basis: 25%;; flex-shrink: 1;"></span>`; 
        }

        html += `<input type="range" class="jspsych-slider" value="${start_value_for_slider_i}" min="${trial.min}" max="${trial.max}" step="${trial.step}" id="jspsych-html-slider-response-response-${i}" style="flex-grow: 1; min-width: 150px; margin-left:5px; margin-right:5px;"></input>`;

        if (label_set_for_slider_i.length > 1 && label_set_for_slider_i[label_set_for_slider_i.length - 1] !== null && typeof label_set_for_slider_i[label_set_for_slider_i.length - 1] !== 'undefined') {
          html += `<span style="margin-left: 10px; font-size:0.8em; white-space:normal; word-break: break-word; text-align: center; flex-basis: 25%;; flex-shrink: 1;">${label_set_for_slider_i[label_set_for_slider_i.length - 1]}</span>`;
        } else {
           html += `<span style="margin-left: 10px; flex-basis: 25%;; flex-shrink: 1;"></span>`; 
        }
        
        html += '</div>'; 
        html += "</div>"; 
      }      
      html += "</div>"; 
      
      if (trial.prompt !== null) {
        html += trial.prompt;
      }
      
      let tooltip_message = "";
      if (trial.require_movement && num_sliders_to_render > 0) {
          if (num_sliders_to_render === 1) {
              tooltip_message = "Please rate your feelings on the scale using the slider.";
          } else { 
              tooltip_message = "Please rate your feelings on all scales using the sliders.";
          }
      }

      html += `<button id="jspsych-html-slider-response-next" class="jspsych-btn" ${ (trial.require_movement && num_sliders_to_render > 0) ? `disabled title="${tooltip_message}"` : "" }>${trial.button_label}</button>`;
      
      display_element.innerHTML = html;
      
      var response_data = { 
        rt: null,
        values: num_sliders_to_render > 1 ? Array(num_sliders_to_render).fill(trial.slider_start) : trial.slider_start
      };
      
      let slider_movement_status = [];
      if (trial.require_movement && num_sliders_to_render > 0) {
        slider_movement_status = Array(num_sliders_to_render).fill(false);
      }

      const next_button = display_element.querySelector("#jspsych-html-slider-response-next");

      const check_and_enable_button = () => {
        if (trial.require_movement && num_sliders_to_render > 0 && next_button) {
          if (slider_movement_status.every(status => status === true)) {
            next_button.disabled = false;
            next_button.removeAttribute('title'); // Remove tooltip when enabled
          } else {
            next_button.disabled = true;
          }
        }
      };

      if (num_sliders_to_render > 0) {
        for (let i = 0; i < num_sliders_to_render; i++) {
          const slider_element = display_element.querySelector(`#jspsych-html-slider-response-response-${i}`);
          if (slider_element) {
            const initialValue = slider_element.valueAsNumber;
             if (num_sliders_to_render > 1) {
                if(Array.isArray(response_data.values)) response_data.values[i] = initialValue;
            } else {
                response_data.values = initialValue;
            }

            const handle_interaction = () => {
              const currentValue = slider_element.valueAsNumber;
              if (num_sliders_to_render > 1) {
                 if(Array.isArray(response_data.values)) response_data.values[i] = currentValue;
              } else {
                response_data.values = currentValue;
              }
              if (trial.require_movement) {
                slider_movement_status[i] = true;
                check_and_enable_button();
              }
            };
            slider_element.addEventListener("input", handle_interaction); 
            slider_element.addEventListener("change", handle_interaction); 
          }
        }
      }
      
      const end_trial = () => {
        var trialdata = {
          rt: response_data.rt,
          stimulus: trial.stimulus,
          slider_start: trial.slider_start, 
          response: null 
        };

        if (num_sliders_to_render > 0) {
            if (num_sliders_to_render > 1) {
                trialdata.response = Array(num_sliders_to_render).fill(null);
                for (let i = 0; i < num_sliders_to_render; i++) {
                    const slider_el = display_element.querySelector(`#jspsych-html-slider-response-response-${i}`);
                    trialdata.response[i] = slider_el ? slider_el.valueAsNumber : trial.slider_start; 
                }
            } else { 
                const slider_el = display_element.querySelector("#jspsych-html-slider-response-response-0");
                trialdata.response = slider_el ? slider_el.valueAsNumber : trial.slider_start;
            }
        } else {
            trialdata.response = null; 
        }
        
        this.jsPsych.finishTrial(trialdata);
      };

      if (next_button) {
          next_button.addEventListener("click", () => {
            var endTime = performance.now();
            response_data.rt = Math.round(endTime - startTime);
            
            if (num_sliders_to_render > 0) {
                if (num_sliders_to_render > 1) {
                    const current_slider_values = [];
                    for (let i = 0; i < num_sliders_to_render; i++) {
                        const slider_el = display_element.querySelector(`#jspsych-html-slider-response-response-${i}`);
                        current_slider_values.push(slider_el ? slider_el.valueAsNumber : trial.slider_start);
                    }
                    response_data.values = current_slider_values;
                } else {
                    const slider_el = display_element.querySelector("#jspsych-html-slider-response-response-0");
                    response_data.values = slider_el ? slider_el.valueAsNumber : trial.slider_start;
                }
            } else {
                 response_data.values = null;
            }

            if (trial.response_ends_trial) {
              end_trial();
            } else {
              if (next_button) next_button.disabled = true;
            }
          });
      }
      
      if (trial.stimulus_duration !== null) {
        this.jsPsych.pluginAPI.setTimeout(() => {
          const stimulus_el = display_element.querySelector("#jspsych-html-slider-response-stimulus");
          if (stimulus_el) stimulus_el.style.visibility = "hidden";
        }, trial.stimulus_duration);
      }
      
      if (trial.trial_duration !== null) {
        this.jsPsych.pluginAPI.setTimeout(end_trial, trial.trial_duration);
      }
      
      var startTime = performance.now();
    }
    simulate(trial, simulation_mode, simulation_options, load_callback) {
      if (simulation_mode == "data-only") {
        load_callback();
        this.simulate_data_only(trial, simulation_options);
      }
      if (simulation_mode == "visual") {
        this.simulate_visual(trial, simulation_options, load_callback);
      }
    }
    create_simulation_data(trial, simulation_options) {
      const default_data = {
        stimulus: trial.stimulus,
        slider_start: trial.slider_start,
        response: this.jsPsych.randomization.randomInt(trial.min, trial.max),
        rt: this.jsPsych.randomization.sampleExGaussian(500, 50, 1 / 150, true)
      };
      const data = this.jsPsych.pluginAPI.mergeSimulationData(default_data, simulation_options);
      this.jsPsych.pluginAPI.ensureSimulationDataConsistency(trial, data);
      return data;
    }
    simulate_data_only(trial, simulation_options) {
      const data = this.create_simulation_data(trial, simulation_options);
      this.jsPsych.finishTrial(data);
    }
    simulate_visual(trial, simulation_options, load_callback) {
      const data = this.create_simulation_data(trial, simulation_options);
      const display_element = this.jsPsych.getDisplayElement();
      this.trial(display_element, trial);
      load_callback();
      if (data.rt !== null) {
        const el = display_element.querySelector("input[type='range']");
        setTimeout(() => {
          this.jsPsych.pluginAPI.clickTarget(el);
          el.valueAsNumber = data.response;
        }, data.rt / 2);
        this.jsPsych.pluginAPI.clickTarget(display_element.querySelector("button"), data.rt);
      }
    }
  }

  return HtmlSliderResponsePlugin;

})(jsPsychModule);
