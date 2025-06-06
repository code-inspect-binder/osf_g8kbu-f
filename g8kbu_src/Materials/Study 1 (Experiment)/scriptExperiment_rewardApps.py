#------------------------------------Smartphone Rewards: Python Script---------------------------------#
# Script for visual-search task used in:
# Smartphone Apps are not Associated with Reward, but the Smartphone Itself may be Rewarding
# Link to OSF: 
# Authors: [blinded]
#------------------------------------------------------------------------------------------------------#

from os.path import join, isfile
from psychopy import visual, core, event, gui
from random import shuffle, choice
from math import cos, sin, pi, atan2, degrees, sqrt

#----------------------------------------Participant Information----------------------------------------#
def ppInformation():
    info = {'Participant Number':0, 'Condition':0, 'Age':0, 'Gender': ['Female', 'Male']}
    infoDlg = gui.DlgFromDict(dictionary=info, title='Participant Information')
    if infoDlg.OK:
        pp = info['Participant Number']
        condition = int(info['Condition'])   
        age = info['Age']
        gender = info['Gender']
        f = open('Output//PP'+str(pp)+'_RewardApps_Demographics.txt', 'a')
        f.write("{}\t{}\t{}\t{}\n".format(pp, condition, age, gender))
        f.close()
    else:
        core.quit()
    return pp, condition

#---------------------------------------------Instructions----------------------------------------------#
def displayInstruction(win, instructionFile):
    instructionFolder = 'Instructions' # folder where the instructions are saved 
    file = join(instructionFolder, instructionFile)
    instructionImg = visual.ImageStim(win, image=file+'.png')
    instructionImg.draw()
    win.flip()
    if isfile(join(instructionFolder, instructionFile)+str('_space.png')): # after some time, a second instruction page allows to go forward with spacebar https://stackoverflow.com/questions/82831/how-do-i-check-whether-a-file-exists-using-python
        core.wait(5)
        instructionImg = visual.ImageStim(win, image=file+'_space.png')
        instructionImg.draw()
        win.flip()
    response = event.waitKeys(keyList = ['space'])

#---------------------------------------------Error Feedback--------------------------------------------#
def displayFeedback(win, feedbackFile, duration):
    instructionFolder = 'Instructions' 
    file = join(instructionFolder, feedbackFile)
    feedbackImg = visual.ImageStim(win, image=file+'.png')
    feedbackImg.draw()
    win.flip()
    core.wait(duration)

#----------------------------------------------Count Down-----------------------------------------------#
def countDown(win, seconds, message):
    for i in range(seconds):
        countDownMessage=visual.TextStim(win, text=str(message)+ str(seconds-i) + ' ...', color='black', height=40)
        countDownMessage.draw()
        win.flip()
        core.wait(1)
        
#------------------------------------------------Stimuli------------------------------------------------#
def drawFix(win, pretrial, height):
    fix = visual.TextStim(win, '+', pos=(0,0), height=height, units='pix', color = 'black')
    fix.draw()
    if pretrial == 1:
        fixDuration = [0.4, 0.5, 0.6]
        win.flip()
        core.wait(choice(fixDuration)) # randomly choose how long the fixation cross is first displayed

def drawCircle(win, radius, position):
    circle = visual.Circle(win, radius=radius, edges=300, lineWidth=2, lineColor='black', pos=position)
    circle.draw()

def drawDiamond(win, radius, position):
    diamond = visual.Polygon(win, lineColor='black', edges=4, lineWidth=2, radius=radius, pos=position)
    diamond.draw()

def drawTilted(win, radius, position):
    direction = {'right' : [(position[0]-sqrt(radius**2/2), position[1]-sqrt(radius**2/2)), (position[0]+sqrt(radius**2/2), position[1]+sqrt(radius**2/2))],
                 'left' : [(position[0]+sqrt(radius**2/2), position[1]-sqrt(radius**2/2)), (position[0]-sqrt(radius**2/2), position[1]+sqrt(radius**2/2))]}
    # the visual.Line function requires a starting and end point from which we randomly choose, so we put those starting points in a dictionnary
    # the starting points are relative to the position of the surrounding shape, so start and end point have to be 'centered' around the position of the shape
    # the diagonal line is longer than the vertical or horizontal, so it has be transformed in order to be the same length
    randomDirection = choice(list(direction)) # to be able to choose keys, dictionnary has to be a list https://stackoverflow.com/questions/18552001/accessing-dict-keys-element-by-index-in-python3/18552025#18552025
    line = visual.Line(win, lineColor='black', start=direction[randomDirection][0], end=direction[randomDirection][1], lineWidth=5) # start and end point according to the randomly chosen direction, which refers back to the positions in the dictionnary https://stackoverflow.com/questions/18552001/accessing-dict-keys-element-by-index-in-python3/18552025#18552025
    line.draw()

def drawNonTilted(win, radius, position):
    direction = {'horizontal' : [(position[0]-radius,position[1]), (position[0]+radius,position[1])], 'vertical' : [(position[0],position[1]-radius), (position[0],position[1]+radius)]}
    # same as above: the starting and end points of the lines are relative to the position of the shape
    randomDirection = choice(list(direction))
    line = visual.Line(win, lineColor='black', start=direction[randomDirection][0], end=direction[randomDirection][1], lineWidth=5)
    line.draw()
    return randomDirection

def drawIcon(win, icon, radius, position):
    iconsFolder = 'Icons'
    file = join(iconsFolder, icon)
    iconImg = visual.ImageStim(win, image=file, size = (55, 55), pos=position) # radius gets doubled so that overall it corresponds to the size of the shapes
    iconImg.draw()

def stimSize(degr):     # from http://osdoc.cogsci.nl/3.1/visualangle/#LstDeg --> calculating pixels based on visual angles
                        # source for degrees: https://link.springer.com/content/pdf/10.3758%2Fs13414-017-1289-6.pdf
    h = 30.5 # monitor height in cm; display height is 30.5, the entire monitor with bezels is 33.5
    d = 50 # distance between monitor and participant in cm
    r = 1080 # vertical resolution of the monitor
    size_in_deg = degr # the stimulus size in degrees (5 for imaginary circle, 2.3 for shapes, 0.5 for fixation cross)
    
    # calculate the number of degrees that correspond to a single pixel
    deg_per_px = degrees(atan2(.5*h, d)) / (.5*r)

    # Calculate the size of the stimulus in degrees
    size_in_px = size_in_deg / deg_per_px

    return size_in_px/2 # divided by two because we use the output as radius for the shapes

def pointOnCircle(radius, angle):   # from https://stackoverflow.com/questions/35402609/point-on-circle-base-on-given-angle and https://stackoverflow.com/questions/5300938/calculating-the-position-of-points-in-a-circle
    center = [0,0]

    x = center[0] + (radius * cos(angle))
    y = center[1] + (radius * sin(angle)) # calculates the x,y coordinates on the imaginary circle

    return (x,y)

def getPositions(): # iterate through the points on the imaginary circle
    positions = []
    
    for i in range(1,7):
        angle = pi/3 * i + pi/6 # calculates the x,y coordinates on the circle, such that they are aligned along the vertical axis (uses radians, not degrees)
        positions.append(pointOnCircle(stimSize(5*2), angle))

    return positions

#------------------------------------------------Output------------------------------------------------#    
def writeHeader(pp):
    outputFile = open('Output//PP'+str(pp)+'_RewardApps.txt', 'a')
    variableNames = ('PP', 'Condition', 'BlockType', 'BlockNumber', 'TrialNumber', 'TrialType', ' UniqueShape', 'Icon', 'Direction', 'RespKey', 'RespRT', 'RespCorrect')
    outputFile.write('%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n' %variableNames)
    outputFile.close()

def writeOutput(pp, condition, blockType, blockNumber, trialNumber, trialType, uniqueShape, icon, direction, respKey, respRT, respCorrect):
    outputFile = open('Output//PP'+str(pp)+'_RewardApps.txt', 'a')
    dataPerTrial=(pp, condition, blockType, blockNumber, trialNumber, trialType, uniqueShape, icon, direction, respKey, respRT, respCorrect)
    outputFile.write('{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\n'.format(*dataPerTrial))
    outputFile.close()

#--------------------------------------Counterbalancing and Trials-------------------------------------#
def mappingKeys(pp):
    if pp % 2 == 1:
        mapping = {'z' : 'vertical', 'm' : 'horizontal'}
    else:
        mapping = {'z' : 'horizontal', 'm' : 'vertical'}
    return mapping

def trialLists():
    # practice trials
    socialApps = ['facebook.png', 'instagram.png', 'messenger.png', 'snapchat.png', 'whatsapp.png']
    socialApps1 = ['facebook1.png', 'instagram1.png', 'messenger1.png', 'snapchat1.png', 'whatsapp1.png']
    neutralApps = ['calculator.png', 'clock.png', 'notes.png', 'settings.png', 'weather.png']

    # practice trials would be much too long if each combination was possible, so we pick them randomly from their respective categories
    practiceTrials = []
    for i in range(3):
        practiceTrials.append((choice(socialApps), 'diamond' ,'distractor'))
        practiceTrials.append((choice(socialApps), 'circle' ,'distractor'))

        practiceTrials.append((choice(socialApps1), 'diamond' ,'distractor1'))
        practiceTrials.append((choice(socialApps1), 'circle' ,'distractor1'))

    for i in range(6):
        practiceTrials.append((choice(neutralApps), 'diamond' ,'neutral'))
        practiceTrials.append((choice(neutralApps), 'circle' ,'neutral')) 

    shuffle(practiceTrials)

    # experimental trials, divided in 5 block with 96 trials each (needs the slash to signal multiline https://stackoverflow.com/questions/4172448/is-it-possible-to-break-a-long-line-to-multiple-lines-in-python)
    expTrials = \
    12 * [('whatsapp.png', 'diamond', 'distractor')] + 12 * [('facebook.png', 'diamond', 'distractor')] + 12 * [('messenger.png', 'diamond', 'distractor')] + 12 * [('instagram.png', 'diamond', 'distractor')] + 12 * [('snapchat.png', 'diamond', 'distractor')] + \
    12 * [('whatsapp.png', 'circle', 'distractor')] + 12 * [('facebook.png', 'circle', 'distractor')] + 12 * [('messenger.png', 'circle', 'distractor')] + 12 * [('instagram.png', 'circle', 'distractor')] + 12 * [('snapchat.png', 'circle', 'distractor')] + \
    12 * [('whatsapp1.png', 'diamond', 'distractor1')] + 12 * [('facebook1.png', 'diamond', 'distractor1')] + 12 * [('messenger1.png', 'diamond', 'distractor1')] + 12 * [('instagram1.png', 'diamond', 'distractor1')] + 12 * [('snapchat1.png', 'diamond', 'distractor1')] + \
    12 * [('whatsapp1.png', 'circle', 'distractor1')] + 12 * [('facebook1.png', 'circle', 'distractor1')] + 12 * [('messenger1.png', 'circle', 'distractor1')] + 12 * [('instagram1.png', 'circle', 'distractor1')] + 12 * [('snapchat1.png', 'circle', 'distractor1')] + \
    24 * [('settings.png', 'diamond', 'neutral')] + 24 * [('clock.png', 'diamond', 'neutral')] + 24 * [('calculator.png', 'diamond', 'neutral')] + 24 * [('notes.png', 'diamond', 'neutral')] + 24 * [('weather.png', 'diamond', 'neutral')] + \
    24 * [('settings.png', 'circle', 'neutral')] + 24 * [('clock.png', 'circle', 'neutral')] + 24 * [('calculator.png', 'circle', 'neutral')] + 24 * [('notes.png', 'circle', 'neutral')] + 24 * [('weather.png', 'circle', 'neutral')]

    shuffle(expTrials)

    trialBlocks = {} # blocks of trials are stored in a dictionnary according to index
    k = 0
    for i in range(1, len(expTrials)/96+1):
        trialBlocks['trialBlock' + str(i)] = expTrials[k:k+96]
        k += 96

    return practiceTrials, trialBlocks  

#--------------------------------------------Trial Function-------------------------------------------#

def drawTrial(win, pp, condition, keyMapping, blockType, blockNumber, trialNumber, icon, uniqueShape, trialType, radius, positions):

    # determine position of unique shape randomly
    shuffle(positions)

    drawFix(win, 1, stimSize(0.5*2)) # times two because the output of stimSize is half of the pixels (radius)
    drawFix(win, 0, stimSize(0.5*2))

    # draws the nontarget (nonunique) shapes        
    for aposition in range(5): # first five positions are not unique

        if uniqueShape == 'diamond': # determines which of the two is the unique shape
            drawCircle(win, radius, positions[aposition])
        else:
            drawDiamond(win, radius, positions[aposition])
       
        drawTilted(win, radius*0.6, positions[aposition]) # draws the tilted lines in the non-unique shapes

    # draws the icon
    drawIcon(win, icon, radius*0.5, positions[choice(range(5))]) # randomly chooses a position that is not the last position (the last position is the target shape)

    # draws the unique shape
    if uniqueShape == 'diamond':
        drawDiamond(win, radius, positions[5])
    else:
        drawCircle(win, radius, positions[5])
        
    direction = drawNonTilted(win, radius*0.6, positions[5]) # draws vertical or horizontal line in unique shapes, function returns direction and stores it in variable for output
        
    win.flip()

    event.clearEvents()
    clock = core.Clock()
    response = event.waitKeys(maxWait=1.5, keyList=keyMapping.keys(), timeStamped=clock)

    if response:
        respKey = response[0][0]
        respRT = response[0][1]
        
        if keyMapping[respKey] == direction:
            respCorrect = 1
            displayFeedback(win, 'correct', 1)
        else:
            respCorrect = 0
            displayFeedback(win, 'incorrect', 1)

    else:
        respKey = respRT = respCorrect = 'NA'
        displayFeedback(win, 'faster', 1)

    writeOutput(pp, condition, blockType, blockNumber, trialNumber, trialType, uniqueShape, icon, direction, respKey, respRT, respCorrect)

    win.flip()
    core.wait(0.5) # intertrial interval

#--------------------------------------------Manipulation Checks-------------------------------------------#

def manipulationCheck1(win, pp, condition):

    urgeScale = visual.RatingScale(win, scale = '', low = 0, high = 100, tickMarks = (0,100), labels = ('Not at all', 'Extremely'),
                             textColor = 'black', lineColor = 'black', acceptText = 'Next', precision = 1, marker = 'triangle', markerStart = 50,
                             singleClick = False, showValue = False, size = 1.2, textSize = 0.4, acceptSize = 2)

    stim = visual.TextStim(win, '', color = 'black', alignHoriz = 'center', alignVert = 'center', height = 30, font = 'Calibri')

    event.Mouse(visible = True)
         
    ratingScale = urgeScale
    
    while ratingScale.noResponse:
        stim.setText('Right now, to what extend do you feel an urge to check your phone?')
        stim.draw()
        ratingScale.draw()
        win.flip()

    rating = ratingScale.getRating()

    ratingScale.reset()

    event.Mouse(visible = False)

    return rating

def manipulationCheck2(win, pp, condition, urge):

    yesNoScale = visual.RatingScale(win, textColor = 'black', choices = ['Yes', 'No'], size = 1, textSize = 0.5, marker = 'hover')
            
    manChecksSocial = ['WhatsApp', 'Facebook', 'Facebook Messenger', 'Snapchat', 'Instagram']
    manChecksNeutral = ['Calculator', 'Clock', 'Notes', 'Settings', 'Weather']
    manChecksFalse = ['E-Mail', 'Calendar', 'Netflix', 'Dropbox', 'Skype', 'iMessage', 'Twitter', 'Spotify', 'AppStore', 'Contacts']

    allIcons = manChecksSocial + manChecksNeutral + manChecksFalse
    shuffle(allIcons)

    stim = visual.TextStim(win, '', color = 'black', alignHoriz = 'center', alignVert = 'center', height = 30, font = 'Calibri')

    event.Mouse(visible = True)

    outputFile = open('Output//PP' + str(pp) + '_ManCheck_RewardApps.txt', 'a')
    outputFile.write('{}\t{}\t{}\t{}\t{}\n'.format('PP', 'Condition', 'Urge' 'App', 'Response', 'Correct'))
    outputFile.close()
    
    for anIcon in allIcons:
              
        ratingScale = yesNoScale
        
        while ratingScale.noResponse:
            stim.setText('Did you see this app icon during the task?\n\n' + str(anIcon))
            stim.draw()
            ratingScale.draw()
            win.flip()

        rating = ratingScale.getRating()

        if (anIcon not in manChecksFalse and rating == 'Yes') or (anIcon in manChecksFalse and rating == 'No') :
            correct = 1
        else:
            correct = 0
        
        outputFile = open('Output//PP' + str(pp) + '_ManCheck_RewardApps.txt', 'a')
        outputFile.write('{}\t{}\t{}\t{}\t{}\t{}\n'.format(pp, condition, urge, anIcon, rating, correct))
        outputFile.close()
        core.wait(0.1)
        ratingScale.reset()
        
    event.Mouse(visible = False)

#--------------------------------------------Main Function-------------------------------------------#
def mainTask():
    
    # get participant information and assign to variables
    (pp, condition) = ppInformation()

    # keymapping is counterbalanced according to PP number
    keyMapping = mappingKeys(pp)

    # assign positions for shape
    positions = getPositions()
    
    # get trial lists
    (practiceTrials, trialBlocks) = trialLists()

    # initiate window
    win = visual.Window(color='white', units='pix', fullscr=1, screen = 1)

    # disable mouse
    event.Mouse(visible=False)
        
    # instructions and first manipulation check
    displayInstruction(win, 'intro1')

    urge = manipulationCheck1(win, pp, condition)

    for apage in range(2, 4):
        displayInstruction(win, 'intro' + str(apage))

    if keyMapping['z'] == 'vertical': # instructions based on counterbalancing
        displayInstruction(win, 'zvertical')
    else:
        displayInstruction(win, 'mvertical')

    displayInstruction(win, 'intro4')

    # write the header for data file
    writeHeader(pp)

    # run practice trials
    blockType = 'practice'
    blockNumber = 1
    trialNumber = 1
    countDown(win, 5, 'Next block starting in ')
    for atrial in practiceTrials:
        drawTrial(win, pp, condition, keyMapping, blockType, blockNumber, trialNumber, atrial[0], atrial[1], atrial[2], stimSize(2.3)*1.5, positions)
        trialNumber += 1

    # run experimental blocks
    displayInstruction(win, 'intro5')

    blockType = 'experimental'
    blockNumber += 1

    for ablock in trialBlocks.keys():
        countDown(win, 15, 'Take a break for at least \n')
        displayInstruction(win, 'break')
        countDown(win, 5, 'Next block starting in ')
        for atrial in trialBlocks[ablock]: # assesses the values (so the trial lists) assoicated with the key we iterate through
            drawTrial(win, pp, condition, keyMapping, blockType, blockNumber, trialNumber, atrial[0], atrial[1], atrial[2], stimSize(2.3)*1.5, positions)
            trialNumber += 1
        blockNumber += 1        

    # second manipulation check
    displayInstruction(win, 'mancheckApps')
    manipulationCheck2(win, pp, condition, urge)
    
    # end screen
    displayInstruction(win, 'end')
      
    win.close()
    core.quit()

mainTask()

