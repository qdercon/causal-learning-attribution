var jsPsychHtmlButtonResponseCA = (function (jspsych) {
    'use strict';

    var version = "2.0.0"; // updated from original version at https://github.com/agnesnorbury/cognitive-mechanisms-psychotherapy/blob/main/causal-attribution-restructuring/public/js/jspsych-7.2.1/plugin-html-button-response-ca.js
    const info = {
        name: "html-button-response-ca",
        version,
        parameters: {
            /** The HTML content to be displayed. */
            stimulus: {
                type: jspsych.ParameterType.HTML_STRING,
                default: void 0
            },
            /** Labels for the buttons. Each different string in the array will generate a different button. */
            choices: {
                type: jspsych.ParameterType.STRING,
                default: void 0,
                array: true
            },
            /**
             * A function that generates the HTML for each button in the `choices` array. The function gets the string and index of the item in the `choices` array and should return valid HTML. If you want to use different markup for each button, you can do that by using a conditional on either parameter. The default parameter returns a button element with the text label of the choice.
             */
            button_html: {
                type: jspsych.ParameterType.FUNCTION,
                default: function(choice, choice_index) {
                    return `<button class="jspsych-btn">${choice}</button>`;
                }
            },
            /** This string can contain HTML markup. Any content here will be displayed below the stimulus. The intention is that it can be used to provide a reminder about the action the participant is supposed to take (e.g., which key to press). */
            prompt: {
                type: jspsych.ParameterType.HTML_STRING,
                default: null
            },
            /** How long to display the stimulus in milliseconds. */
            stimulus_duration: {
                type: jspsych.ParameterType.INT,
                default: null
            },
            /** How long to wait for a response before ending the trial in milliseconds. */
            trial_duration: {
                type: jspsych.ParameterType.INT,
                default: null
            },
            /** Layout style for buttons: 'grid' or 'flex'. */
            button_layout: {
                type: jspsych.ParameterType.STRING,
                default: "flex"
            },
            /** Number of rows in the button grid (if button_layout is 'grid'). */
            grid_rows: {
                type: jspsych.ParameterType.INT,
                default: 1
            },
            /** Number of columns in the button grid (if button_layout is 'grid'). */
            grid_columns: {
                type: jspsych.ParameterType.INT,
                default: null
            },
            /** If true, the trial ends immediately after a response. */
            response_ends_trial: {
                type: jspsych.ParameterType.BOOL,
                default: true
            },
            /** How long the button will delay enabling in milliseconds. */
            enable_button_after: {
                type: jspsych.ParameterType.INT,
                default: 0
            },
            /** How long to display the trial info before participant can make a response. */
            time_before_choice: {
                type: jspsych.ParameterType.INT,
                pretty_name: "ms to wait before response",
                default: 0,
            },
            /** How long to show the trial info after a response (for response_ends_trial=true only). */
            time_after_choice: {
                type: jspsych.ParameterType.INT,
                pretty_name: "ms to wait after response",
                default: 0,
            },
        },
        data: {
            /** The response time in milliseconds. */
            rt: {
                type: jspsych.ParameterType.INT
            },
            /** The index of the button pressed. */
            response: {
                type: jspsych.ParameterType.INT
            },
            /** The stimulus HTML. */
            stimulus: {
                type: jspsych.ParameterType.HTML_STRING
            }
        },
        // prettier-ignore
        citations: {
            "apa": "de Leeuw, J. R., Gilbert, R. A., & Luchterhandt, B. (2023). jsPsych: Enabling an Open-Source Collaborative Ecosystem of Behavioral Experiments. Journal of Open Source Software, 8(85), 5351. https://doi.org/10.21105/joss.05351 ",
            "bibtex": '@article{Leeuw2023jsPsych, 	author = {de Leeuw, Joshua R. and Gilbert, Rebecca A. and Luchterhandt, Bj{\\" o}rn}, 	journal = {Journal of Open Source Software}, 	doi = {10.21105/joss.05351}, 	issn = {2475-9066}, 	number = {85}, 	year = {2023}, 	month = {may 11}, 	pages = {5351}, 	publisher = {Open Journals}, 	title = {jsPsych: Enabling an {Open}-{Source} {Collaborative} {Ecosystem} of {Behavioral} {Experiments}}, 	url = {https://joss.theoj.org/papers/10.21105/joss.05351}, 	volume = {8}, }  '
        }
    };

    class HtmlButtonResponsePluginCA {
        constructor(jsPsych) {
            this.jsPsych = jsPsych;
            this.buttonElements = []; // Store button elements
        }

        static {
            this.info = info;
        }

        trial(display_element, trial) {
            // Clear previous button elements
            this.buttonElements = [];
            display_element.innerHTML = ''; // Clear display

            // --- Display Stimulus ---
            const stimulusElement = document.createElement("div");
            stimulusElement.id = "jspsych-html-button-response-stimulus";
            stimulusElement.innerHTML = trial.stimulus;
            display_element.appendChild(stimulusElement);

            // --- Display Buttons ---
            const buttonGroupElement = document.createElement("div");
            buttonGroupElement.id = "jspsych-html-button-response-btngroup-ca"; // Keep original ID if needed

            // Apply layout styles
            if (trial.button_layout === "grid") {
                buttonGroupElement.classList.add("jspsych-btn-group");
                if (trial.grid_rows === null && trial.grid_columns === null) {
                    console.error("Error in html-button-response-ca plugin: You cannot set `grid_rows` to `null` without providing a value for `grid_columns`.");
                }
                const n_cols = trial.grid_columns === null ? Math.ceil(trial.choices.length / trial.grid_rows) : trial.grid_columns;
                const n_rows = trial.grid_rows === null ? Math.ceil(trial.choices.length / trial.grid_columns) : trial.grid_rows;
                buttonGroupElement.style.display = "grid"; // Add this line
                buttonGroupElement.style.gridTemplateColumns = `repeat(${n_cols}, 1fr)`;
                buttonGroupElement.style.gridTemplateRows = `repeat(${n_rows}, 1fr)`;
            } else if (trial.button_layout === "flex") {
                buttonGroupElement.classList.add("jspsych-btn-group");
                buttonGroupElement.style.display = "flex";
                buttonGroupElement.style.flexWrap = "wrap";
            }

            // Determine button HTML handling
            let button_html_provider;
            if (Array.isArray(trial.button_html) && trial.button_html.length === trial.choices.length) {
                button_html_provider = (choice, index) => trial.button_html[index];
            } else if (typeof trial.button_html === 'string') {
                button_html_provider = (choice, index) => trial.button_html.replace(/%choice%/g, choice);
            } else if (typeof trial.button_html === 'function') {
                button_html_provider = trial.button_html;
            } else {
                console.error("Error in html-button-response-ca plugin: button_html must be a string, function, or an array of strings with the same length as choices.");
                button_html_provider = (choice, index) => `<button class="jspsych-btn">${choice}</button>`;
            }

            // Create and add buttons
            for (const [choiceIndex, choice] of trial.choices.entries()) {
                let button_html_final = button_html_provider(choice, choiceIndex);
                const buttonWrapper = document.createElement("div"); // Wrapper for parsing HTML
                buttonWrapper.innerHTML = button_html_final;
                const buttonElement = buttonWrapper.firstElementChild; // Get the actual button element
                
                if (buttonElement) {
                    buttonElement.dataset.choice = choiceIndex.toString();
                    buttonElement.classList.add("jspsych-html-button-response-button-ca"); // Add a class for easier selection if needed
                    buttonGroupElement.appendChild(buttonElement);
                    this.buttonElements.push(buttonElement); // Store for potential later use (e.g. enabling/disabling)
                }
            }
            display_element.appendChild(buttonGroupElement);

            // --- Display Prompt ---
            if (trial.prompt !== null) {
                const promptElement = document.createElement("div");
                promptElement.id = "jspsych-html-button-response-prompt";
                promptElement.innerHTML = trial.prompt;
                display_element.appendChild(promptElement);
            }

            // --- Timing and Response Handling ---
            const start_time = performance.now();
            let response = {
                rt: null,
                button: null,
            };
            let response_has_been_made = false; // Flag to prevent multiple processing

            // Function to handle response
            const after_response = (choice) => {
                if (response_has_been_made) return; // Prevent multiple responses
                response_has_been_made = true;

                // Add "responded" class to the chosen button
                response.button = parseInt(choice);
                // Add "disabled-choice" class to the unchosen buttons
                this.buttonElements.forEach((btn, index) => {
                    if (index !== response.button) {
                        btn.classList.add("disabled-choice");
                    } else {
                        btn.classList.add("responded");
                    }
                });

                // measure rt
                const end_time = performance.now();
                const rt = Math.round(end_time - start_time);
                response.rt = rt;

                // Clear timeouts related to enabling/clicking buttons
                this.jsPsych.pluginAPI.clearAllTimeouts();

                if (trial.response_ends_trial) {
                    // Wait for time_after_choice before ending
                    this.jsPsych.pluginAPI.setTimeout(end_trial, trial.time_after_choice);
                }
                // If response_ends_trial is false, the trial_duration timeout will handle ending.
            };

            // Function to end the trial
            const end_trial = () => {
                // kill any remaining setTimeout handlers
                this.jsPsych.pluginAPI.clearAllTimeouts();

                // gather the data to store for the trial
                var trial_data = {
                    rt: response.rt,
                    stimulus: trial.stimulus,
                    response: response.button,
                    // Include other data points as needed
                };

                // clear the display
                display_element.innerHTML = "";

                // move to the next trial
                this.jsPsych.finishTrial(trial_data);
            };

            // --- Setup Timers ---

            // Function to add event listeners to buttons
            const add_button_listeners = () => {
                this.buttonElements.forEach((button_el) => {
                    if (button_el) { // Ensure button_el exists
                        button_el.addEventListener("click", () => {
                            // Check if the button is effectively disabled via a class or attribute if necessary
                            // For example, if a 'disabled' class is used instead of the attribute for styling reasons
                            if (button_el.hasAttribute("disabled") || button_el.classList.contains("jspsych-disabled")) {
                                return;
                            }
                            const choiceAttribute = button_el.dataset.choice;
                            // The following line is removed as per user request:
                            // buttonGroupElement.querySelector(`[data-choice="${choiceAttribute}"]`).classList.add("responded");
                            after_response(parseInt(choiceAttribute));
                        });
                    }
                });
            };
            
            // 1. Initial Button State (Disabled if enable_button_after > 0 or time_before_choice > 0)
            const initial_buttons_disabled = trial.enable_button_after > 0 || trial.time_before_choice > 0;
            if (initial_buttons_disabled) {
                this.buttonElements.forEach(button => button.disabled = true);
            }

            // 2. Timer to Enable Buttons (respects both enable_button_after and time_before_choice)
            const actual_enable_time = Math.max(trial.enable_button_after, trial.time_before_choice);

            if (actual_enable_time > 0) {
                this.jsPsych.pluginAPI.setTimeout(() => {
                    this.buttonElements.forEach(button => button.disabled = false);
                    add_button_listeners(); // Add listeners once buttons are enabled
                }, actual_enable_time);
            } else {
                add_button_listeners(); // Add listeners immediately if no delay
            }

            // Hide stimulus if stimulus_duration is set
            if (trial.stimulus_duration !== null) {
                this.jsPsych.pluginAPI.setTimeout(() => {
                    stimulusElement.style.visibility = "hidden";
                }, trial.stimulus_duration);
            }

            // 5. Trial Duration Timer
            if (trial.trial_duration !== null) {
                this.jsPsych.pluginAPI.setTimeout(end_trial, trial.trial_duration);
            }
        }

        // --- Simulation Methods (largely unchanged, check clickTarget selector) ---
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
            // Add time_before_choice and enable_button_after to the simulated RT
            const base_rt = this.jsPsych.randomization.sampleExGaussian(500, 50, 1 / 150, true);
            const effective_enable_time = Math.max(trial.enable_button_after, trial.time_before_choice); // Response can only happen after both delays

            const default_data = {
                stimulus: trial.stimulus,
                rt: base_rt + effective_enable_time,
                response: this.jsPsych.randomization.randomInt(0, trial.choices.length - 1)
            };

            const data = this.jsPsych.pluginAPI.mergeSimulationData(default_data, simulation_options);

            // Ensure rt is not less than the minimum possible time
            data.rt = Math.max(data.rt, effective_enable_time);

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
                // Use the more robust selector targeting the data-choice attribute within the button group
                const clickTargetSelector = `#${this.info.name}-btngroup-ca [data-choice="${data.response}"]`;
                const targetElement = display_element.querySelector(clickTargetSelector);

                if (targetElement) {
                    // Click happens after the effective enable time
                    const clickTime = Math.max(data.rt, trial.time_before_choice, trial.enable_button_after);
                    this.jsPsych.pluginAPI.clickTarget(targetElement, clickTime);
                } else {
                    console.warn(`Simulation warning: Could not find button element with selector: ${clickTargetSelector}`);
                }
            }
        }
    }

    return HtmlButtonResponsePluginCA;

})(jsPsychModule);