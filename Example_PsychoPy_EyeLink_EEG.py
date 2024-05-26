### Example PsychoPy+EyeLink+EEG Experiment (Concurrent Pupillometry and EEG) ###
# Built:    November 3, 2023
# Updated:  November 10, 2023
# Author:   Drew J. McLaughlin
#
# Notes:
#   1) See "Walkthrough_Building EyeLink-Compatible PsychoPy Experiments.docx"

### Load packages ###

from __future__ import division
from __future__ import print_function

import pylink
import pyo
import os
import platform
import random
import time
import sys
from EyeLinkCoreGraphicsPsychoPy import EyeLinkCoreGraphicsPsychoPy
from psychopy import visual, core, event, monitors, gui
from psychopy.constants import (NOT_STARTED, STARTED, PLAYING, PAUSED, 
                                STOPPED, FINISHED, PRESSED, RELEASED, FOREVER)
from psychopy.hardware import keyboard
from PIL import Image  # for preparing the Host backdrop image
from string import ascii_letters, digits

from psychopy import visual, microphone, sound, event, core, data, gui, os
import pandas as pd

from psychopy.hardware import labhackers


### Standard PsychoPy Prep  ###

# Show only critical log message in the PsychoPy console
from psychopy import logging
logging.console.setLevel(logging.CRITICAL)

# Set this variable to True if you use the built-in retina screen as your
# primary display device on macOS. If have an external monitor, set this
# variable True if you choose to "Optimize for Built-in Retina Display"
# in the Displays preference settings.
use_retina = False

# Set this variable to True to run the script in "Dummy Mode"
dummy_mode = False

# Set this variable to True to run the task in full screen mode
# It is easier to debug the script in non-fullscreen mode
full_screen = True


### EyeLink Files Set-up: EDF Filename, Local Data Folder, Counterbalances, Etc. ###

# The EDF data filename should not exceed 8 alphanumeric characters
# use ONLY number 0-9, letters, & _ (underscore) in the filename
# Use of counterbalances will vary by experiment -- edit as needed!
edf_fname = 'TEST'
counterbalance = '1'

# Prompt user to specify an EDF data filename 
# before we open a fullscreen window
# NOTE: In this version, we allow the user to input a counterbalance number,
#       which determines which Excel file is opened for stimuli/conditions.
#       See "Removing/Editing Counterbalances" in walkthrough if you need
#       guidance on removing/changing this feature.
dlg_title = 'Enter EDF File Name'
dlg_prompt = 'Please enter a file name with 8 or fewer characters\n' + \
             '[letters, numbers, and underscore].'
dlg_prompt2 = 'Please enter the assigned counterbalance \n' + \
              '[value 1 or 2].'

# loop until we get a valid filename
while True:
    dlg = gui.Dlg(dlg_title)
    dlg.addText(dlg_prompt)
    dlg.addField('File Name:', edf_fname)
    dlg.addText(dlg_prompt2)
    dlg.addField('Counterbalance:', counterbalance)
    
    # show dialog and wait for OK or Cancel
    ok_data = dlg.show()
    if dlg.OK:  # if ok_data is not None
        print('EDF data filename: {}'.format(ok_data[0]))
    else:
        print('user cancelled')
        core.quit()
        sys.exit()

    # get the strings entered by the experimenter
    tmp_str = dlg.data[0]
    counterbalance = dlg.data[1]
    # strip trailing characters, ignore the ".edf" extension
    edf_fname = tmp_str.rstrip().split('.')[0]

    # check if the filename is valid (length <= 8 & no special char)
    allowed_char = ascii_letters + digits + '_'
    if not all([c in allowed_char for c in edf_fname]):
        print('ERROR: Invalid EDF filename')
    elif len(edf_fname) > 8:
        print('ERROR: EDF filename should not exceed 8 characters')
    else:
        break

# Set up a folder to store the EDF data files and the associated resources
# e.g., files defining the interest areas used in each trial
results_folder = 'results'
if not os.path.exists(results_folder):
    os.makedirs(results_folder)

# We download EDF data file from the EyeLink Host PC to the local hard
# drive at the end of each testing session, here we rename the EDF to
# include session start date/time
time_str = time.strftime("_%Y_%m_%d_%H_%M", time.localtime())
session_identifier = edf_fname + time_str

# create a folder for the current testing session in the "results" folder
session_folder = os.path.join(results_folder, session_identifier)
if not os.path.exists(session_folder):
    os.makedirs(session_folder)

# Data file name stem = absolute path + name; later add .psyexp, .csv, .log, etc
_thisDir = os.path.dirname(os.path.abspath(__file__))
os.chdir(_thisDir)
filename = _thisDir + os.sep + u'data/%s_%s' %(str(edf_fname), 'CRAVE')

# An ExperimentHandler isn't essential but helps with data saving
thisExp = data.ExperimentHandler(name='FamPup', version='V1',
    extraInfo=None, runtimeInfo=None,
    savePickle=True, saveWideText=True,
    dataFileName=filename)
# save a log file for detail verbose info
logFile = logging.LogFile(filename+'.log', level=logging.EXP)
logging.console.setLevel(logging.WARNING)  # this outputs to the screen, not a file


### Beginning of "Steps" (for "talking" to EyeLink computer and running experiment) ###
# The process is commented with "Steps" 1-9

### Step 1: Connect to the EyeLink Host PC ###
# The Host IP address, by default, is "100.1.1.1".
# the "el_tracker" objected created here can be accessed through the Pylink
# Set the Host PC address to "None" (without quotes) to run the script
# in "Dummy Mode"
if dummy_mode:
    el_tracker = pylink.EyeLink(None)
else:
    try:
        el_tracker = pylink.EyeLink("100.1.1.1")
    except RuntimeError as error:
        print('ERROR:', error)
        core.quit()
        sys.exit()


### Step 2: Open an EDF data file on the Host PC
edf_file = edf_fname + ".EDF"
try:
    el_tracker.openDataFile(edf_file)
except RuntimeError as err:
    print('ERROR:', err)
    # close the link if we have one open
    if el_tracker.isConnected():
        el_tracker.close()
    core.quit()
    sys.exit()

# Add a header text to the EDF file to identify the current experiment name
# This is OPTIONAL. If your text starts with "RECORDED BY " it will be
# available in DataViewer's Inspector window by clicking
# the EDF session node in the top panel and looking for the "Recorded By:"
# field in the bottom panel of the Inspector.
preamble_text = 'RECORDED BY %s' % os.path.basename(__file__)
el_tracker.sendCommand("add_file_preamble_text '%s'" % preamble_text)


### Step 3: Configure the tracker
#
# Put the tracker in offline mode before we change tracking parameters
el_tracker.setOfflineMode()

# Get the software version:  1-EyeLink I, 2-EyeLink II, 3/4-EyeLink 1000,
# 5-EyeLink 1000 Plus, 6-Portable DUO
eyelink_ver = 0  # set version to 0, in case running in Dummy mode
if not dummy_mode:
    vstr = el_tracker.getTrackerVersionString()
    eyelink_ver = int(vstr.split()[-1].split('.')[0])
    # print out some version info in the shell
    print('Running experiment on %s, version %d' % (vstr, eyelink_ver))

# File and Link data control
# what eye events to save in the EDF file, include everything by default
file_event_flags = 'LEFT,RIGHT,FIXATION,SACCADE,BLINK,MESSAGE,BUTTON,INPUT'
# what eye events to make available over the link, include everything by default
link_event_flags = 'LEFT,RIGHT,FIXATION,SACCADE,BLINK,BUTTON,FIXUPDATE,INPUT'
# what sample data to save in the EDF data file and to make available
# over the link, include the 'HTARGET' flag to save head target sticker
# data for supported eye trackers
if eyelink_ver > 3:
    file_sample_flags = 'LEFT,RIGHT,GAZE,HREF,RAW,AREA,HTARGET,GAZERES,BUTTON,STATUS,INPUT'
    link_sample_flags = 'LEFT,RIGHT,GAZE,GAZERES,AREA,HTARGET,STATUS,INPUT'
else:
    file_sample_flags = 'LEFT,RIGHT,GAZE,HREF,RAW,AREA,GAZERES,BUTTON,STATUS,INPUT'
    link_sample_flags = 'LEFT,RIGHT,GAZE,GAZERES,AREA,STATUS,INPUT'
el_tracker.sendCommand("file_event_filter = %s" % file_event_flags)
el_tracker.sendCommand("file_sample_data = %s" % file_sample_flags)
el_tracker.sendCommand("link_event_filter = %s" % link_event_flags)
el_tracker.sendCommand("link_sample_data = %s" % link_sample_flags)

# Optional tracking parameters
# Sample rate, 250, 500, 1000, or 2000, check your tracker specification
# if eyelink_ver > 2:
#     el_tracker.sendCommand("sample_rate 1000")
# Choose a calibration type, H3, HV3, HV5, HV13 (HV = horizontal/vertical),
el_tracker.sendCommand("calibration_type = HV9")
# Set a gamepad button to accept calibration/drift check target
# You need a supported gamepad/button box that is connected to the Host PC
el_tracker.sendCommand("button_function 5 'accept_target_fixation'")


### Step 4: Set up a graphics environment for calibration
#
# Open a window, be sure to specify monitor parameters
mon = monitors.Monitor('myMonitor', width=53.0, distance=70.0)
win = visual.Window(fullscr=full_screen,
                    monitor=mon,
                    winType='pyglet',
                    units='pix')

# get the native screen resolution used by PsychoPy
scn_width, scn_height = win.size
# resolution fix for Mac retina displays
if 'Darwin' in platform.system():
    if use_retina:
        scn_width = int(scn_width/2.0)
        scn_height = int(scn_height/2.0)

# Pass the display pixel coordinates (left, top, right, bottom) to the tracker
# see the EyeLink Installation Guide, "Customizing Screen Settings"
el_coords = "screen_pixel_coords = 0 0 %d %d" % (scn_width - 1, scn_height - 1)
el_tracker.sendCommand(el_coords)

# Write a DISPLAY_COORDS message to the EDF file
# Data Viewer needs this piece of info for proper visualization, see Data
# Viewer User Manual, "Protocol for EyeLink Data to Viewer Integration"
dv_coords = "DISPLAY_COORDS  0 0 %d %d" % (scn_width - 1, scn_height - 1)
el_tracker.sendMessage(dv_coords)

# Configure a graphics environment (genv) for tracker calibration
genv = EyeLinkCoreGraphicsPsychoPy(el_tracker, win)
print(genv)  # print out the version number of the CoreGraphics library

# Set background and foreground colors for the calibration target
# in PsychoPy, (-1, -1, -1)=black, (1, 1, 1)=white, (0, 0, 0)=mid-gray
foreground_color = (-1, -1, -1)
background_color = (0, 0, 0)
genv.setCalibrationColors(foreground_color, background_color)

# Set up the calibration target
# Use a picture as the calibration target
genv.setTargetType('picture')
genv.setPictureTarget(os.path.join('images', 'fixTarget.bmp'))

# Configure the size of the calibration target (in pixels)
# this option applies only to "circle" and "spiral" targets
# genv.setTargetSize(24)

# Beeps to play during calibration, validation and drift correction
# parameters: target, good, error
#     target -- sound to play when target moves
#     good -- sound to play on successful operation
#     error -- sound to play on failure or interruption
# Each parameter could be ''--default sound, 'off'--no sound, or a wav file
genv.setCalibrationSounds('', '', '')

# resolution fix for macOS retina display issues
if use_retina:
    genv.fixMacRetinaDisplay()

# Request Pylink to use the PsychoPy window we opened above for calibration
pylink.openGraphicsEx(genv)

# Create some handy timers
globalClock = core.Clock()  # to track the time since experiment started
routineTimer = core.Clock()  # to track time remaining of each (possibly non-slip) routine 

# More standard psychopy settings
endExpNow = False  # flag for 'escape' or other condition => quit the exp
frameTolerance = 0.001  # how close to onset before 'same' frame

# create a default keyboard (e.g. to check for escape)
defaultKeyboard = keyboard.Keyboard(backend='event')


### Experiment-Specific Settings and Variables ###

# Keyboard set-up
ioConfig = {}
ioConfig['Keyboard'] = dict(use_keymap='psychopy')
defaultKeyboard = keyboard.Keyboard(backend='iohub')
key_resp = keyboard.Keyboard()

# Sound set-up
trial_audio = sound.Sound('A', secs=1, stereo = True, hamming = True, name = 'trial_audio')
trial_audio.setVolume(1.0)

# Initialize instructions and other text for the experiment
Welcome = ( u'Bienvenido!\n\nPulsa la barra espaciadora para practicar.')
Begin = (u'La sesión de práctica ha finalizado.\n\nCuando estés listo para comenzar la tarea, pulsa la barra espaciadora.')
Break = (u'Ahora puedes tomarte un breve descanso.\nPulsa la barra espaciadora para continuar.')
End = (u'Este es el final del estudio.\nMuchas gracias por haber participado!')
fixation_cross = visual.TextStim(win=win, name='fixation_cross', text='+',
                          color='white', colorSpace='rgb',
                          height=100)
fixation_circle = visual.TextStim(win=win, name='fixation_circle', text='o',
                          color='white', colorSpace='rgb',
                          height=100)


### Establish Connection to EEG  ###
# In this example, just one trigger is needed per trial (at onset of sentence)
p_port = labhackers.USB2TTL8()
p_port.status = None


### Step 5: Defining Key Functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
# define a few helper functions for trial handling
def clear_screen(win):
    """ clear up the PsychoPy window"""

    win.fillColor = genv.getBackgroundColor()
    win.flip()

def show_msg(win, text, wait_for_keypress=True, time_show=None, height=20, mouse_visible=False):
    """ Show task instructions on screen"""
    
    win.mouseVisible = mouse_visible
    
    msg = visual.TextStim(win, text,
                          color=genv.getForegroundColor(),
                          wrapWidth=scn_width/2,
                          height=height)
    clear_screen(win)
    if time_show is not None:
        baseline_onset_time = core.getTime()
        while core.getTime() - baseline_onset_time < time_show:
            msg.draw()
            win.flip()
            
    else:
        msg.draw()
        win.flip()

    # wait indefinitely, terminates upon any key press
    if wait_for_keypress:
        event.waitKeys()
        clear_screen(win)

def terminate_task():
    """ Terminate the task gracefully and retrieve the EDF data file

    file_to_retrieve: The EDF on the Host that we would like to download
    win: the current window used by the experimental script
    """

    el_tracker = pylink.getEYELINK()

    if el_tracker.isConnected():
        # Terminate the current trial first if the task terminated prematurely
        error = el_tracker.isRecording()
        if error == pylink.TRIAL_OK:
            abort_trial()

        # Put tracker in Offline mode
        el_tracker.setOfflineMode()

        # Clear the Host PC screen and wait for 500 ms
        el_tracker.sendCommand('clear_screen 0')
        pylink.msecDelay(500)

        # Close the edf data file on the Host
        el_tracker.closeDataFile()

        # Show a file transfer message on the screen
        msg = 'EDF data is transferring from EyeLink Host PC...'
        show_msg(win, msg, wait_for_keypress=False)

        # Download the EDF data file from the Host PC to a local data folder
        # parameters: source_file_on_the_host, destination_file_on_local_drive
        local_edf = os.path.join(session_folder, session_identifier + '.EDF')
        try:
            el_tracker.receiveDataFile(edf_file, local_edf)
        except RuntimeError as error:
            print('ERROR:', error)

        # Close the link to the tracker.
        el_tracker.close()

    # close the PsychoPy window
    win.close()

    # quit PsychoPy
    core.quit()
    sys.exit()

def abort_trial():
    """Ends recording """

    el_tracker = pylink.getEYELINK()

    # Stop recording
    if el_tracker.isRecording():
        # add 100 ms to catch final trial events
        pylink.pumpDelay(100)
        el_tracker.stopRecording()

    # clear the screen
    clear_screen(win)
    # Send a message to clear the Data Viewer screen
    bgcolor_RGB = (116, 116, 116)
    el_tracker.sendMessage('!V CLEAR %d %d %d' % bgcolor_RGB)

    # send a message to mark trial end
    el_tracker.sendMessage('TRIAL_RESULT %d' % pylink.TRIAL_ERROR)

    return pylink.TRIAL_ERROR

def run_driftCorrect():
    
    # get a reference to the currently active EyeLink connection
    el_tracker = pylink.getEYELINK()

    # put the tracker in the offline mode first
    el_tracker.setOfflineMode()

    # clear the host screen before we draw the backdrop
    el_tracker.sendCommand('clear_screen 0')

    # send a "TRIALID" message to mark the start of a trial, see Data
    # Viewer User Manual, "Protocol for EyeLink Data to Viewer Integration"
    el_tracker.sendMessage('TRIALID %d' % trial_index)

    # record_status_message : show some info on the Host PC
    # here we show how many trial has been tested
    status_msg = 'TRIAL number %d' % trial_index
    el_tracker.sendCommand("record_status_message '%s'" % status_msg)

    # put tracker in idle/offline mode before recording
    el_tracker.setOfflineMode()

    # Start recording
    # arguments: sample_to_file, events_to_file, sample_over_link,
    # event_over_link (1-yes, 0-no)
    try:
        el_tracker.startRecording(1, 1, 1, 1)
    except RuntimeError as error:
        print("ERROR:", error)
        abort_trial()
        return pylink.TRIAL_ERROR

    # Allocate some time for the tracker to cache some samples
    pylink.pumpDelay(100)
    
    # Message at start of drift check
    el_tracker.sendMessage('drift_correct_onset')
    
    # drift check
    # we recommend drift-check at the beginning of each trial
    # the doDriftCorrect() function requires target position in integers
    # the last two arguments:
    # draw_target (1-default, 0-draw the target then call doDriftCorrect)
    # allow_setup (1-press ESCAPE to recalibrate, 0-not allowed)
    #
    # Skip drift-check if running the script in Dummy Mode
    while not dummy_mode:
        # terminate the task if no longer connected to the tracker or
        # user pressed Ctrl-C to terminate the task
        if (not el_tracker.isConnected()) or el_tracker.breakPressed():
            terminate_task()
            return pylink.ABORT_EXPT

        # drift-check and re-do camera setup if ESCAPE is pressed
        try:
            error = el_tracker.doDriftCorrect(int(scn_width/2.0),
                                              int(scn_height/2.0), 1, 1)
            # break following a success drift-check
            if error is not pylink.ESC_KEY:
                break
        except:
            pass
    
    # Message at end of drift correct
    el_tracker.sendMessage('drift_correct_offset')
    
    # stop recording; add 100 msec to catch final events before stopping
    pylink.pumpDelay(100)
    el_tracker.stopRecording()


### FUNCTION CONTAINING TRIAL PROCEDURE ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###

def run_trial(trial_pars, trial_index):
    """ Helper function specifying the events that will occur in a single trial
    trial_pars - a list containing trial parameters, e.g.,
                ['condition', 'image']
    trial_index - record the order of trial presentation in the task
    """
    
    ### Unpacking all of the trial parameters (same names as excel file header)
    # see "Messages for Trial-Specific Information Logging"
    target_speaker = trial_pars[0]
    target_accent = trial_pars[1]
    target_audio = trial_pars[2]
    target_sentence = trial_pars[3]
    target_number = trial_pars[4]
    trial_duration = trial_pars[5]
    eeg_condition_trigger = trial_pars[6]
    
    # load audio
    trial_audio.setSound(os.path.join('audio', target_audio))
    
    # get a reference to the currently active EyeLink connection
    el_tracker = pylink.getEYELINK()

    # put the tracker in the offline mode first
    el_tracker.setOfflineMode()

    # clear the host screen before we draw the backdrop
    el_tracker.sendCommand('clear_screen 0')

    # send a "TRIALID" message to mark the start of a trial, see Data
    # Viewer User Manual, "Protocol for EyeLink Data to Viewer Integration"
    el_tracker.sendMessage('TRIALID %d' % trial_index)

    # record_status_message : show some info on the Host PC
    # here we show how many trial has been tested
    status_msg = 'TRIAL number %d' % trial_index
    el_tracker.sendCommand("record_status_message '%s'" % status_msg)

    # put tracker in idle/offline mode before recording
    el_tracker.setOfflineMode()

    ### Start recording ###
    # arguments: sample_to_file, events_to_file, sample_over_link,
    # event_over_link (1-yes, 0-no)
    try:
        el_tracker.startRecording(1, 1, 1, 1)
    except RuntimeError as error:
        print("ERROR:", error)
        abort_trial()
        return pylink.TRIAL_ERROR

    # Allocate some time for the tracker to cache some samples
    pylink.pumpDelay(100)


    ### PLAY AUDIO while displaying FIXATION CIRCLE ###
    clear_screen(win)
    win.mouseVisible = False
    audio_onset_time = core.getTime()  # record the audio onset time
    
    continueRoutine = True
    routineForceEnded = False

    # Send a message to clear the Data Viewer screen, get it ready for
    # drawing the pictures during visualization
    bgcolor_RGB = (116, 116, 116)
    el_tracker.sendMessage('!V CLEAR %d %d %d' % bgcolor_RGB)

    # keep track of which components have finished
    play_soundComponents = [trial_audio, p_port]
    for thisComponent in play_soundComponents:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    frameN = -1
    routineTimer.reset()

    # Play audio: Allows for 3s silence before target sentence and 3s after 
    #             (plus some padding after to keep math simple)
    while continueRoutine and routineTimer.getTime() < 10:
        
        fixation_circle.setAutoDraw(True)
        fixation_circle.draw(win=win)
        
        # get current time
        t = routineTimer.getTime()
        tThisFlip = win.getFutureFlipTime(clock=routineTimer)
        tThisFlipGlobal = win.getFutureFlipTime(clock=None)
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        
        # start/stop trial_audio
        if trial_audio.status == NOT_STARTED and tThisFlip >= 3.0-frameTolerance:
            # keep track of start time/frame for later
            trial_audio.frameNStart = frameN  # exact frame index
            trial_audio.tStart = t  # local t and not account for scr refresh
            trial_audio.tStartRefresh = tThisFlipGlobal  # on global time
            # add timestamp to datafile
            thisExp.addData('trial_audio.started', tThisFlipGlobal)
            trial_audio.play(when=win)  # sync with win flip
            win.flip()
            el_tracker.sendMessage('audio_onset')
        if trial_audio.status == STARTED:
            # is it time to stop? (based on global clock, using actual start)
            if tThisFlipGlobal > trial_audio.tStartRefresh + 10.0-frameTolerance:
                # keep track of stop time/frame for later
                trial_audio.tStop = t  # not accounting for scr refresh
                trial_audio.frameNStop = frameN  # exact frame index
                # add timestamp to datafile
                thisExp.timestampOnFlip(win, 'trial_audio.stopped')
                trial_audio.stop()
        
        # *p_port* updates
        if p_port.status == NOT_STARTED and trial_audio.status == STARTED:
            # keep track of start time/frame for later
            p_port.frameNStart = frameN  # exact frame index
            p_port.tStart = t  # local t and not account for scr refresh
            p_port.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(p_port, 'tStartRefresh')  # time at next scr refresh
            p_port.status = STARTED
            win.callOnFlip(p_port.setData, int(eeg_condition_trigger))
        if p_port.status == STARTED:
            # is it time to stop? (based on global clock, using actual start)
            if tThisFlipGlobal > p_port.tStartRefresh + 0.5-frameTolerance:
                # keep track of stop time/frame for later
                p_port.tStop = t  # not accounting for scr refresh
                p_port.frameNStop = frameN  # exact frame index
                win.timeOnFlip(p_port, 'tStopRefresh')  # time at next scr refresh
                p_port.status = FINISHED
                win.callOnFlip(p_port.setData, int(0))
        
        # abort the current trial if the tracker is no longer recording
        error = el_tracker.isRecording()
        if error is not pylink.TRIAL_OK:
            el_tracker.sendMessage('tracker_disconnected')
            abort_trial()
            return error

        # check keyboard events
        for keycode, modifier in event.getKeys(modifiers=True):
            # Abort a trial if "ESCAPE" is pressed
            if keycode == 'escape':
                el_tracker.sendMessage('trial_skipped_by_user')
                # clear the screen
                clear_screen(win)
                # abort trial
                abort_trial()
                return pylink.SKIP_TRIAL

            # Terminate the task if Ctrl-c
            if keycode == 'c' and (modifier['ctrl'] is True):
                el_tracker.sendMessage('terminated_by_user')
                terminate_task()
                return pylink.ABORT_EXPT
        
        # check for quit (typically the Esc key)
        if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
            core.quit()
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
        

    # clear the screen
    clear_screen(win)
    el_tracker.sendMessage('blank_screen')
    routineTimer.reset()
    # send a message to clear the Data Viewer screen as well
    el_tracker.sendMessage('!V CLEAR 128 128 128')
    
    # --- Ending Routine "play_sound" ---
    for thisComponent in play_soundComponents:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    trial_audio.stop()  # ensure sound has stopped at end of routine
    fixation_circle.setAutoDraw(False)  # ensure circle is removed from screen
    
    
    ### PRESENT FIXATION_CROSS and wait for spacebar ###
    
    clear_screen(win)
    fixation_cross.setAutoDraw(True)
    fixation_cross.draw(win=win)
    win.flip()
    el_tracker.sendMessage('Response_period_onset')
    
    continueRoutine = True
    routineForceEnded = False
    
    # Some keyboard set up
    key_resp.keys = []
    key_resp.rt = []
    _key_resp_allKeys = []

    # Send a message to clear the Data Viewer screen, get it ready for
    # drawing the pictures during visualization
    bgcolor_RGB = (116, 116, 116)
    el_tracker.sendMessage('!V CLEAR %d %d %d' % bgcolor_RGB)

    # keep track of which components have finished
    responseComponents = [key_resp]
    for thisComponent in responseComponents:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    frameN = -1

    # Shows the fixation cross until spacebar is pressed
    while continueRoutine:
        
        # get current time
        t = routineTimer.getTime()
        tThisFlip = win.getFutureFlipTime(clock=routineTimer)
        tThisFlipGlobal = win.getFutureFlipTime(clock=None)
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        
        # Display cross throughout
        fixation_cross.setAutoDraw(True)
        fixation_cross.draw(win=win)
        
        # *key_resp* updates
        waitOnFlip = False
        if key_resp.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            key_resp.frameNStart = frameN  # exact frame index
            key_resp.tStart = t  # local t and not account for scr refresh
            key_resp.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(key_resp, 'tStartRefresh')  # time at next scr refresh
            # add timestamp to datafile
            thisExp.timestampOnFlip(win, 'key_resp.started')
            key_resp.status = STARTED
            # keyboard checking starting
            waitOnFlip = True
            win.callOnFlip(key_resp.clock.reset)
            win.callOnFlip(key_resp.clearEvents, eventType = 'keyboard')
        if key_resp.status == STARTED and not waitOnFlip:
            theseKeys = key_resp.getKeys(keyList=['space'], waitRelease=False)
            _key_resp_allKeys.extend(theseKeys)
            if len(_key_resp_allKeys):
                key_resp.keys = _key_resp_allKeys[-1].name
                key_resp.rt = _key_resp_allKeys[-1].rt
                # a response ends the routine
                continueRoutine = False
        
        # abort the current trial if the tracker is no longer recording
        error = el_tracker.isRecording()
        if error is not pylink.TRIAL_OK:
            el_tracker.sendMessage('tracker_disconnected')
            abort_trial()
            return error

        # check keyboard events
        for keycode, modifier in event.getKeys(modifiers=True):
            # Abort a trial if "ESCAPE" is pressed
            if keycode == 'escape':
                el_tracker.sendMessage('trial_skipped_by_user')
                # clear the screen
                clear_screen(win)
                # abort trial
                abort_trial()
                return pylink.SKIP_TRIAL

            # Terminate the task if Ctrl-c
            if keycode == 'c' and (modifier['ctrl'] is True):
                el_tracker.sendMessage('terminated_by_user')
                terminate_task()
                return pylink.ABORT_EXPT
        
        # check for quit (typically the Esc key)
        if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
            core.quit()
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            routineForceEnded = True
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in responseComponents:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
            
    # store data for thisExp (ExperimentHandler)
    thisExp.nextEntry()

    # clear the screen
    clear_screen(win)
    fixation_cross.setAutoDraw(False)  # ensure cross is removed from screen
    el_tracker.sendMessage('blank_screen')
    routineTimer.reset()
    # send a message to clear the Data Viewer screen as well
    el_tracker.sendMessage('!V CLEAR 128 128 128')
    
    
    ### Send MESSAGES to EDF data file ###
    # Record trial variables to the EDF data file, for instructions, see 
    # "Messages for Trial-Specific Information Logging"
    el_tracker.sendMessage('!V TRIAL_VAR target_speaker %s' % target_speaker)
    el_tracker.sendMessage('!V TRIAL_VAR target_accent %s' % target_accent)
    el_tracker.sendMessage('!V TRIAL_VAR target_audio %s' % target_audio)
    el_tracker.sendMessage('!V TRIAL_VAR target_sentence %s' % target_sentence)
    el_tracker.sendMessage('!V TRIAL_VAR target_number %s' % target_number)
    el_tracker.sendMessage('!V TRIAL_VAR trial_duration %s' % trial_duration)
    el_tracker.sendMessage('!V TRIAL_VAR counterbalance %s' % str(counterbalance))
    
    # Send message to note end of data collection for trial 
    el_tracker.sendMessage('!V TRIAL END')

    # stop recording; add 100 msec to catch final events before stopping
    pylink.pumpDelay(100)
    el_tracker.stopRecording()

    # clear the screen
    clear_screen(win)

    # send a 'TRIAL_RESULT' message to mark the end of trial, see Data
    # Viewer User Manual, "Protocol for EyeLink Data to Viewer Integration"
    el_tracker.sendMessage('TRIAL_RESULT %d' % pylink.TRIAL_OK)
    
    thisExp.nextEntry()


### Step 6: Set up the camera and calibrate the tracker ###

# Show the task instructions
task_msg = '\nPress Ctrl-C to if you need to quit the task early\n'
if dummy_mode:
    task_msg = task_msg + '\nNow, press ENTER to start the task'
else:
    task_msg = task_msg + '\nPress ENTER twice to calibrate tracker'
show_msg(win, task_msg)

# skip this step if running the script in Dummy Mode
if not dummy_mode:
    try:
        el_tracker.doTrackerSetup()
    except RuntimeError as err:
        print('ERROR:', err)
        el_tracker.exitCalibration()


### Step 7: Randomize and Run Experimental Trials ###
# NOTE:     See "Customizing Experiment Randomization, Break, Etc."
#           Most of the sections below call functions defined in Step 4

# Get list of all trials based on assigned counterbalance
trial_list = pd.read_csv("spreadsheets\CB" + str(counterbalance) + ".csv")

# Get list of practice trials
practice_trial_list = pd.read_csv("spreadsheets\Practice.csv")

# Randomize the order of the trial list
trial_list_shuffled = trial_list.sample(frac = 1)

# Save this participant´s randomized trial list
trial_list.to_csv("data" + "\ExperimentName" + edf_fname + "_trial_list.csv")

# Show welcome screen
show_msg(win, Welcome, wait_for_keypress = True)

# Practice trials
values1 = range(0,2)
for trial_index in values1:
    trial_pars = practice_trial_list.iloc[trial_index]
    run_trial(trial_pars, trial_index)

# Show pre-experiment screen
show_msg(win, Begin, wait_for_keypress = True)

# Iterate through each row (each trial) and use run_trial function 
values2 = range(0,5)
for trial_index in values2:
    trial_pars = trial_list_shuffled.iloc[trial_index]
    run_trial(trial_pars, trial_index)

# Take a half-way break
show_msg(win, Break, wait_for_keypress = True)

# Back to the task
values3 = range(5,10)
for trial_index in values3:
    trial_pars = trial_list_shuffled.iloc[trial_index]
    run_trial(trial_pars, trial_index)

# Show end screen
show_msg(win, End, wait_for_keypress = True)


### Step 8: Disconnect, download the EDF file, then terminate task ###
terminate_task()

