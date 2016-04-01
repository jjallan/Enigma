import itertools
import pickle
import random
import re

class Enigma:
    '''Not yet implemented, ready for implementing the "complete" puzzle with orientations included
    - would then also move relevant methods of Turnstile back up into this class.'''

    def __init__(self):
        pass

class Turnstile(Enigma):
    '''A representation of a Turnstile, an instance of the Enigma puzzle developed by Douglas Engel.
    The puzzle consists of two wheels made of pieces, such that each wheel has 12 pieces: 6 almost
    triangular wedges, "corners", and 6 almost rectangular pieces, "edges", the puzzle has 21 pieces
    in total since the two wheels intersect such that, at any time, two corners and an edge reside
    in both wheels.
    A Turnstile has five differently coloured edges and two corners of each colour, the remaining
    six edges are uncoloured, the goal is then, by turning the wheels in 60 degree increments, to
    place each coloured edge between the two corners of the same colour (forming triples), to have
    each triple separated from the next by the uncoloured edges, with a specific colour-triple
    occupying the intersection and the other triples in a given order.
    To change the colours change self.colourNames, the order is, for a solved puzzle:
    [colourless, bottom-left, top-left, intersection, bottom-right, top-right]
    '''

    def __init__(self):
        Enigma.__init__(self)
        self.colourNames = ["grey", "yellow", "blue", "red", "orange", "green"]
        self.setSolved()

    def getSolvedState(self):
        '''Returns the state of a solved Turnstile.
        - a list of colourName indexes starting at the edge at the bottom right of the left wheel,
          working clockwise around the left wheel to the bottom corner of the intersection then
          anticlockwise around the right wheel (forming a figure of eight).
        ''' 
        return [0,1,1,1,0,2,2,2,0,3,3,3,0,4,4,4,0,5,5,5,0] # the colours of the pieces like so:
        #       e,c,e,c,e,c,e,c,e,c,e,c,e,c,e,c,e,c,e,c,e - edge or corner?
        #       l,l,l,l,l,l,l,l,l,i,i,i,r,r,r,r,r,r,r,r,r - left wheel, right wheel or intersection?
        #       b,b,b,b,m,t,t,t,t,t,m,b,b,b,b,b,m,t,t,t,t - bottom, middle or top of puzzle?
        #       r,c,l,l,l,l,l,c,r, ... ,l,c,r,r,r,r,r,c,l - left, centre or right of it's wheel?

    def setSolved(self):
        '''Arrange the puzzle into a solved state'''
        self.state = self.getSolvedState()

    def scramble(self):
        '''Randomise the state'''
        edges = self.state[0::2]
        corners = self.state[1::2]
        random.shuffle(edges)
        random.shuffle(corners)
        self.state = [corners.pop() if i % 2 else edges.pop() for i in range(21)]

    def turn(self, humanReadable):
        '''Perform the Singmaster like sequence of turns provided in the string humanReadable
        - each element of the string can be any of L, L', L2, L2', L3, R, R', R2, R2', R3
          (L3' and R3' will also work, as will L1', L0', L777' and so on, even though superfluous).
          The letter represents the wheel to turn: Left (L) or Right (R)
          The number, if present, represents the number of sixth turns to make. No number implies 1.
          An apostrophe (') instructs for an anti-clockwise rather than a clockwise rotation.
          For example "R3 L' R2 L2'" instructs to:
              turn the right wheel half a rotation (three sixths);
              turn the left wheel anticlockwise a sixth;
              turn the right wheel clockwise two sixths; and finally
              turn the left wheel anticlockwise two sixths.
          White space is disregarded and it is case insensitive,
          so, for example, "  \t   r3   l'R2 l2'  ", or
                           "r3l'R2l2" are both the same instruction as above.
        '''
        instructions = getInstructionsFromHumanReadable(humanReadable)
        isLeftWheel = instructions[0]
        for nClockwiseSixtys in instructions[1]:
            self.turnCN(isLeftWheel, nClockwiseSixtys)
            isLeftWheel = not isLeftWheel

    def turnCN(self, isLeftWheel, nClockwiseSixtys):
        '''Updates state by turning a wheel nClockwiseSixtys * 60 degrees clockwise.
        - isLeftWheel: True = turning left wheel; False = turning right wheel.
        - nClockwiseSixtys: degrees turned divided by sixty, or number of 1/6th clockwise turns.
        '''
        nClockwiseSixtys = nClockwiseSixtys % 6
        if nClockwiseSixtys:
            if isLeftWheel:
                newStart = 2 * (6 - nClockwiseSixtys)
                self.state = self.state[newStart:12] + self.state[0:newStart] + self.state[12:]
            else:
                newStart = 2 * nClockwiseSixtys + 9
                self.state = self.state[0:9] + self.state[newStart:] + self.state[9:newStart]

    def turnA1(self, isLeftWheel):
        '''Updates state by turning a wheel 60 degrees anti-clockwise.
        - isLeftWheel: True = turning left wheel; False = turning right wheel.
        '''
        # A simpler turn method for single sixth anti-clockwise turns; the solver only does this.
        if isLeftWheel:
            self.state = self.state[2:12] + self.state[0:2] + self.state[12:]
        else:
            self.state = self.state[0:9] + self.state[19:] + self.state[9:19]


    def __repr__(self):
        return ', '.join('{0}.{1}'.format('C' if i % 2 else 'E', self.colourNames[c])
                         for i, c in enumerate(self.state))


class Avenger(Turnstile):
    '''An Avenger is a Turnstile which is considered solved given any colour-wise permutation.
     - see help(Turnstile)
     - This implementation relabels the state as turns are made such that 1 is the first in the
       state as ordered. The colourNames are also permuted so the representation is kept as if this
       had not happened.'''

    def __init__(self):
        Turnstile.__init__(self)

    def permute(self):
        colours = [0] * 6
        i = 1
        for colour in self.state:
            if colour and not colours[colour]:
                colours[colour] = i
                i += 1
        self.state = [colours[colour] for colour in self.state]
        self.colourNames = [self.colourNames[colours.index(colour)] for colour in range(6)]

    def scramble(self):
        Turnstile.scramble(self)
        self.permute()

    def turn(self, humanReadable):
        Turnstile.turn(self, humanReadable)
        self.permute()

    def turnCN(self, isLeftWheel, nClockwiseSixtys):
        Turnstile.turnCN(self, isLeftWheel, nClockwiseSixtys)
        self.permute()

    def turnA1(self, isLeftWheel):
        Turnstile.turnA1(self, isLeftWheel)
        self.permute()


class AvengerForSolver(Avenger):
    '''An Avenger that does not permute colour names, they are always 0,1,2,3,4,5
    - The SolverBase will work with an Avenger as it's _puzzle, but has no need to permute the
      colour names so this will be a touch faster.'''

    def __init__(self):
        Avenger.__init__(self)
        self.colourNames = [colourName for colourName in range(6)]

    def permute(self):
        colours = [0] * 6
        i = 1
        for colour in self.state:
            if colour and not colours[colour]:
                colours[colour] = i
                i += 1
        self.state = [colours[colour] for colour in self.state]


class SolverBase:
    '''A solver for Enigmas utilising a cache of all states solvable within a bound of wheel turns
    - This is a base class, to solve puzzles use an inherited class.'''
    
    def __init__(self, cachedMoves, onDemandMoves, cacheFilePathPrefix):
        '''Base implementation, see a derived class such as TurnstileSolver or AvengerSolver.'''
        self._cachedMoves = cachedMoves
        self._onDemandMoves = onDemandMoves
        self._cacheFilePathPrefix = cacheFilePathPrefix
        self._cache = [{tuple(self._puzzle.state):-1}]
        if cacheFilePathPrefix:
            for moves in range(1, cachedMoves + 1):
                print("reading file for move depth {0}".format(moves))
                try:
                    self._readCacheFile(moves)
                except IOError:
                    print("- couldn't read file")
                    break
            moves = len(self._cache)
            while moves <= cachedMoves:
                print("calculating cached solutions for move depth {0}".format(moves))
                self._extendCache()
                print("persisting cached solutions for move depth {0}".format(moves))
                self._writeCacheFile(moves)
                moves += 1
        else:
            for moves in range(1, cachedMoves + 1):
                print("calculating cached solutions for move depth {0}".format(moves))
                self._extendCache()

    def getSolution(self, puzzle):
        '''Returns a string containing instructions in a Singmaster style.
        Each instruction will be one of (L, R, L2, R2, L3, R3, L2', R2', L', R')
        The letter represents the wheel to turn, the number represents how many sixth turns to make
        (1 implied if no number is present) and the apostrophe (') represents to make the turn in an
        anticlockwise, rather than clockwise fashion.
        If no solution was found: returns None
        If no turns are required: returns ""
        '''
        instructions = self.getSolutionInstructions(puzzle)
        return getHumanReadableFromInstructions(instructions)

    def getSolutionInstructions(self, puzzle):
        '''Returns instructions for an optimal solution.
        Format is [isFirstWheelLeft, [turnsToMake]]
        If no solution was found: returns None
        If no turns are required: returns []
        '''
        found = self._getDual(puzzle)
        if found:
            onDemandPath, onDemandMoves, cachePath, cacheMoves = found
            onDemandInstructions = getInstructionsFromPath(onDemandPath, onDemandMoves, False)
            cacheInstructions = getInstructionsFromPath(cachePath, cacheMoves, True)
            instructions = catInstructions(onDemandInstructions, cacheInstructions)
        else:
            instructions = None
        return instructions

    def newCacheFilePrefix(self, cacheFilePathPrefix):
        '''Helper - If this solver was created without a cacheFilePathPrefix but one now wishes
        to persist the cache to files for later just use use this method.'''
        self._cacheFilePathPrefix = cacheFilePathPrefix
        for moves in range(len(self._cache)):
            print("persisting cached solutions for move depth {0}".format(moves))
            self.writeCacheFile(moves)

    def _readCacheFile(self, moveDepth):
        '''Populate the cache at the moveDepth provided from the appropriate file.'''
        with open(self._cacheFilePathPrefix + '.{0}'.format(moveDepth), 'rb') as file:
            if len(self._cache) == moveDepth:
                self._cache.append(pickle.load(file))
            else:
                raise ValueError("Overwriting cache not implemented; create a new solver instead.")

    def _writeCacheFile(self, moveDepth):
        '''Persist the cache's statePathDict at the moveDepth provided to the appropriate file.'''
        with open(self._cacheFilePathPrefix + '.{0}'.format(moveDepth), 'wb') as file:
            pickle.dump(self._cache[moveDepth], file, pickle.HIGHEST_PROTOCOL)

    def _iterNextPaths(self, statePathDict, moveDepth):
        '''Iterates through the next puzzle states reachable by turning the next wheel for each of
        the states in the statePathDict, which should be the states reachable by moveDepth moves.
        '''
        nStep = 2 * 5 ** moveDepth
        nEnd = 5 * nStep
        if moveDepth == 0:
            for prevState in statePathDict:
                self._puzzle.state = list(prevState)
                for isLeftWheel in (1, 0):
                    for n in range(isLeftWheel, nEnd, nStep):
                        self._puzzle.turnA1(isLeftWheel)
                        yield n
                    if isLeftWheel:
                        self._puzzle.turnA1(isLeftWheel)
        else:
            for prevState, prevPath in statePathDict.items():
                self._puzzle.state = list(prevState)
                isLeftWheel = (prevPath + moveDepth) % 2
                for n in range(0, nEnd, nStep):
                    self._puzzle.turnA1(isLeftWheel)
                    yield prevPath + n

    def _extendCache(self):
        '''Extends the cache by a moveDepth of 1'''
        moveDepth = len(self._cache) - 1
        statePathDict = self._cache[moveDepth]
        extension = {}
        for path in self._iterNextPaths(statePathDict, moveDepth):
            curState = tuple(self._puzzle.state)
            if not (curState in extension
                    or any(curState in states for states in self._cache)):
                extension[curState] = path
        self._cache.append(extension)

    def _getCachedPath(self, state):
        '''Returns the path and move depth contained in the cache for the state provided if it
        exists, else returns None.
        '''
        for moves, statePathDict in enumerate(self._cache):
            path = statePathDict.get(state, None)
            if path is not None:
                break
        return path, moves
            
    def _getDual(self, puzzle):
        '''Returns the internal format of an optimal solution - the tuple:
        (onDemandPath, onDemandMoves, cachePath, cacheMoves)
        The paths are integers which, given the moves, may be decoded into instructions.'''
        found = None
        for found in self._iterDuals(puzzle):
            break
        return found

    def _iterDuals(self, puzzle):
        '''Iterates through all available pairs of onDemand paths and cache paths that solve the
        puzzle provided along with their move depths, unless a path is in the cache in which case
        it simply yields that one.
        Yields (onDemandPath, onDemandMoveDepth, cachePath, cacheMoveDepth)
        - if cached onDemandPath will be -1 and onDemandMoves will be 0 (i.e. no path & no turns).
        '''
        # N.B. This iterates through possible path pairs by searching the cache then, if need be,
        # incrementally trying move depths from the puzzle state to see if any of those are in the
        # cache. The first found will always be an optimal length solution since the cache is
        # complete up to self._cachedMoves, but, if we want to, we can get alternatives (which may
        # well also include longer length solutions which may then be filtered out).
        state = tuple(puzzle.state)
        cachePath, cacheMoves = self._getCachedPath(state)
        if cachePath is None and self._onDemandMoves:
            onDemand = [{state:-1}]
            for moveDepth in range(self._onDemandMoves):
                curMoveDepth = moveDepth + 1
                extension = {}
                for path in self._iterNextPaths(onDemand[moveDepth], moveDepth):
                    curState = tuple(self._puzzle.state)
                    if not (curState in extension
                            or any(curState in states for states in onDemand)):
                        extension[curState] = path
                        cachePath, cacheMoves = self._getCachedPath(curState)
                        if cachePath is not None:
                            yield path, curMoveDepth, cachePath, cacheMoves
                onDemand.append(extension)
        else:
            yield -1, 0, cachePath, cacheMoves


class TurnstileSolver(SolverBase):
    '''A solver for Turnstiles'''

    def __init__(self, cachedMoves=10, onDemandMoves=8, cacheFilePathPrefix=None):
        '''Create a solver for Turnstiles.
        - Searches to a depth of cachedMoves + onDemandMoves moves, where any wheel turn is a move.
        - More cached moves will make for a faster search but consumes more memory and has a higher
          start up overhead. Yet to observe a Turnstile needing 18 moves. Searches will find optimal
          length solutions at minimal onDemand depth, so having onDemandMoves set higher wont impact
          the general performance (a malformed Turnstile, however, could mean a full onDemand search
          is performed).
        - cacheFilePathPrefix is a file path, a file is then read for each move depth from 1 to
          cachedMoves if it exists; if a depth is reached such that this is not possible then the
          cache is calculated for remaining depths and each is then persisted as a file to disk.
          The files are named as cacheFilePathPrefix followed by .moveDepth (.1, .2, .3, etc.).
        - Since the total number of states for a Turnstile is 6,286,896,000 it is, at least with
          this implementation, unlikely to be feasible to fully cache all state-solution pairs.
        - The solver will work quickly with cachedMoves=10 and onDemandMoves=7 since the vast
          majority of states lie in the 12-15 move range (~96.5%) and take sub-50ms. (The default is
          set to onDemandMoves=8 in case there are 18 move states). With file creation on a 64-bit
          system this solver used 5.81GB of RAM and took 3 minutes to initialise using a single
          Intel i7-3610QM @ 2.3GHz CPU; the files then took 0.98GB of disk space. Recreating this
          solver from the files took 26 seconds.
        '''
        self._puzzle = Turnstile()
        SolverBase.__init__(self, cachedMoves, onDemandMoves, cacheFilePathPrefix)


class AvengerSolver(SolverBase):
    '''A solver for Avengers'''

    def __init__(self, cachedMoves=8, onDemandMoves=7, cacheFilePathPrefix=None):
        '''Create a solver for Avengers.
        - Searches to a depth of cachedMoves + onDemandMoves moves, where any wheel turn is a move.
        - More cached moves will make for a faster search but consumes more memory and has a higher
          start up overhead. Searches will find optimal length solutions at minimal onDemand depth,
          so having onDemandMoves set higher wont impact the general performance (a malformed
          Avenger, however, could mean a full onDemand search is performed).
        - Avengers have a God's number of 15 - there are 11!*10!/6!/5!/2^5 = 52,390,800 states of
          which only 304 require 15 moves (roughly 1 in 172,000).
        - cacheFilePathPrefix is a file path, a file is then read for each move depth from 1 to
          cachedMoves if it exists; if a depth is reached such that this is not possible then the
          cache is calculated for remaining depths and each is then persisted as a file to disk.
          The files are named as cacheFilePathPrefix followed by .moveDepth (.1, .2, .3, etc.).
        - Setting cachedMoves=15 with file creation used 13.82GB of RAM on a 64-bit system, and took
          65 minutes to initialise using a single Intel i7-3610QM @ 2.3GHz CPU; the files then took
          2.64GB of disk space. Recreating this solver from the files took 1 minute 45 seconds.
        - The solver will work quickly with cachedMoves=8 and onDemandMoves=7 since 95.3% of states
          lie in the 10-13 move range, only 3.34% need 14 turns, and 0.000058% need 15 turns.
          With file creation this solver used 55MB of RAM and took 3 seconds to initialise using a
          single Intel i7-3610QM @ 2.3GHz CPU; the files then took 7.8MB of disk space. Recreating
          this solver from the files took < 300ms.
        '''
        self._puzzle = AvengerForSolver()
        SolverBase.__init__(self, cachedMoves, onDemandMoves, cacheFilePathPrefix)
        #
        # For 15 cachedMoves the cache keys (moves) and associated len(value) (counts) are:
        # moves    count   frequency
        #     0        1    0.0000019%
        #     1        2    0.0000038%
        #     2       10    0.0000191%
        #     3       50    0.0000954%
        #     4      234    0.000447%
        #     5     1121    0.00214%
        #     6     5401    0.0103%
        #     7    25546    0.0488%
        #     8   119475    0.228%
        #     9   552577   1.05%
        #    10  2377383   4.54%
        #    11  8631662  16.5%
        #    12 20629489  39.4%
        #    13 18296107  34.9%
        #    14  1751438   3.34%
        #    15      304   0.000580%
        #SUM: 6286896000 100.000000%  (6286896000 = 11! * 10! / 2^5)
        #


def getInstructionsFromPath(path, moves, invert):
    '''Return the instructions encoded by the provided path integer and number of moves.
    Format is [isFirstWheelLeft, [turnsToMake]]
    If path is None: returns None
    If path is negative or moves <= 1: returns [] (instructing no moves)'''
    if path is None:
        result = None
    else:
        result = []
        if moves >= 1 and path >= 0:
            if invert:
                turnLookup = (1, 2, 3, 4, 5)
            else:
                turnLookup = (5, 4, 3, 2, 1)
            startsWithLeftWheel = bool(path % 2)
            path //= 2
            turns = []
            for move in range(moves):
                path, n = divmod(path, 5)
                turns.append(turnLookup[n])
            if invert:
                if moves % 2:
                    result = [startsWithLeftWheel, turns[-1::-1]]
                else:
                    result = [not startsWithLeftWheel, turns[-1::-1]]
            else:
                result = [startsWithLeftWheel, turns]
    return result

def catInstructions(instructions1, instructions2):
    '''Concatenates two instructions, recursively merging at the centre if possible
    - for example catInstructions([True, [1, 2, 2]], [True, [4, 4, 1]]) = [True, [2]] since it
      represents "L R2 L2" + "L2' R2' L" which is really just a long winded "L2"'''
    def doMerges(i1, i2):
        if len(i1) == 2 and len(i1[1]):
            if len(i2) == 2 and len(i2[1]):
                if i2[0] == (i1[0] == len(i1[1]) % 2):
                    mergedMiddleTurn = (i1[1][-1] + i2[1][0]) % 6
                    start = i1[1][:-1]
                    end = i2[1][1:]
                    if mergedMiddleTurn:
                        result = [i1[0], start + [mergedMiddleTurn] + end]
                    else:
                        result = doMerges([i1[0], start], [not i2[0], end])
                else:
                    result = [i1[0], i1[1] + i2[1]]
            else:
                result = [i1[0], i1[1][:]]
        elif len(i2) == 2 and len(i2[1]):
            result = [i2[0], i2[1][:]]
        else:
            result = []
        return result
    if instructions1 is None or instructions2 is None:
        result = None
    else:
        if len(instructions1) == 2 and len(instructions1[1]):
            if not all(i % 6 for i in instructions1[1]):
                raise ValueError("Shouldn't instruct to perform turns that do not actually turn.")
        if len(instructions2) == 2 and len(instructions2[1]):
            if not all(i % 6 for i in instructions2[1]):
                raise ValueError("Shouldn't instruct to perform turns that do not actually turn.")
        result = doMerges(instructions1, instructions2)
    return result

def getHumanReadableFromInstructions(instructions):
    '''Return a Singmaster like string of instructions
       Each instruction will be one of (L, R, L2, R2, L3, R3, L2', R2', L', R')
       The letter represents the wheel to turn, the number represents how many sixth turns to make
       (1 implied if no number is present) and the apostrophe (') represents to make the turn in an
       anticlockwise, rather than clockwise fashion.
       If no instructions is None: returns None
       If instructions are [] or have no turns information (e.g. [True, []): returns ""'''
    if instructions is None:
        result = None
    if len(instructions) == 2 and len(instructions[1]):
        if not all(i % 6 for i in instructions[1]):
            raise ValueError("Shouldn't instruct to perform turns that do not actually turn.")
        if instructions[0]:
            wheels = ("L", "R")
        else:
            wheels = ("R", "L")
        turns = ("!error!", "", "2", "3", "2'", "'")
        return " ".join("{0}{1}".format(
                          wheels[n % 2], turns[turn % 6]) for n, turn in enumerate(instructions[1]))    
    else:
        result = ""

def getInstructionsFromHumanReadable(humanReadable):
    '''Returns instructions from a Singmaster like string
    Handles upper or lower case for the wheels (R/r or L/l) and any valid integer literal for the
    turns'''
    humanInstructions = re.findall("([RrLl]+?)(\-?[0-9]*)('?)", humanReadable)
    if humanInstructions:
        merged = []
        for humanInstruction in humanInstructions:
            if humanInstruction[2]:
                if humanInstruction[1]:
                    n = -int(humanInstruction[1]) % 6
                else:
                    n = 5
            else:
                if humanInstruction[1]:
                    n = int(humanInstruction[1]) % 6
                else:
                    n = 1
            if n:
                w = humanInstruction[0].upper() == "L"
                if merged and merged[-1]:
                    if merged[-1][0] == w:
                        n = (n + merged[-1][1]) % 6
                        if n:
                            merged[-1][1] = n
                        else:
                            merged.pop(-1)
                    else:
                        merged.append([w, n])
                else:
                    merged.append([w, n])
        if merged:
            result = [merged[0][0], [v[1] for v in merged]]
        else:
            result = []
    else:
        result = []
    return result



# Just helpers now...

def invertHumanReadable(humanReadable):
    return getHumanReadableFromInstructions(invertInstructions(getInstructionsFromHumanReadable(
                                                                                    humanReadable)))

def invertInstructions(instructions):
    if len(instructions) == 2 and len(instructions[1]):
        if not all(i % 6 for i in instructions[1]):
            raise ValueError("Shouldn't instruct to perform turns that do not actually turn.")
        result = [instructions[0] == len(instructions[1]) % 2, [6 - n % 6 for n in instructions[1][-1::-1]]]
    else:
        result = []
    return result

