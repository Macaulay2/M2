needs "classes.m2"
needs "methods.m2"

-- originally defined (as null) in evaluate.d
iterator = method(Dispatch => Thing)
next = method()

Iterator = new SelfInitializingType of FunctionClosure
Iterator.synonym = "iterator"

iterator Iterator := identity
next Iterator := iter -> iter()

net Iterator := iter -> if hasAttribute(iter,ReverseDictionary) then net getAttribute(iter,ReverseDictionary) else (
    x := if not (first frames iter)#?0 then () else first first frames iter;
    net FunctionApplication(iterator,
	(if instance(x, String) then format else identity) x))

iterator VisibleList :=
iterator String      := x -> Iterator (
    i := 0;
    () -> (
	if i >= #x then StopIteration
	else (
	    r := x#i;
	    i += 1;
	    r)))

toList Thing := x -> for y in x list y

-- called by map(Expr,Expr) in actors3.d
applyIterator = (iter, f) -> Iterator (
    () -> (
	x := next iter;
	if x === StopIteration then StopIteration
	else f x))

select(Thing, Function) := Iterator => {} >> o -> (X, f) -> (
    if lookup(iterator, class X) === null
    then error "expected argument 1 to be an iterable object";
    iter := iterator X;
    Iterator (
	() -> while true do (
	    x := next iter;
	    if x === StopIteration then return StopIteration;
	    y := f x;
	    if not instance(y, Boolean)
	    then error("select: expected predicate to yield true or false");
	    if y then return x)))

joinIterators = a -> (
    n := #a;
    iters := iterator \ a;
    i := 0;
    Iterator(
	() -> (
	    if i >= n then StopIteration
	    else (
		while (
		    r := next iters#i;
		    r === StopIteration)
		do (
		    i += 1;
		    if i >= n then return StopIteration);
		r))))

Iterator | Iterator := (x, y) -> joinIterators(x, y)

pairsIterator = x -> Iterator (
    iter := iterator x;
    i := 0;
    () -> (
	y := next iter;
	if y === StopIteration then StopIteration
	else (i, (i += 1; y))))

ProgressBar = new Type of MutableHashTable
ProgressBar.synonym = "progressBar"

protect Iterable
protect CurrentIndex
protect StartTime
protect TotalTimeElapsed
protect EstimatedRemainingTime
protect AverageTimePerIteration
new ProgressBar from Iterator := (typeofProgressBar, theIterator) -> (
    new MutableHashTable from {
        symbol Iterable => theIterator,
        symbol TotalIterations => infinity,
        symbol Description => "",
        symbol BarCharacter => "▆",
        symbol cache => new CacheTable from {
            symbol CurrentIndex => 0,
            symbol StartTime => null,               -- When the iterator first starts
            symbol TotalTimeElapsed => null,        -- Total time elapsed since start
            symbol EstimatedRemainingTime => null,  -- Estimated time remaining
            symbol AverageTimePerIteration => null  -- Average time elapsed for each iteration
        }
    }
)
new ProgressBar from List := (typeofProgressBar, theList) -> (
    theBar := new ProgressBar from iterator theList;
    theBar.TotalIterations = #theList;
    theBar
)
new ProgressBar from Set := (typeofProgressBar, theSet) -> (
    theBar := new ProgressBar from iterator toList theSet;
    theBar.TotalIterations = #theSet;
    theBar
)

progressBar = method(
    Options => {
        symbol TotalIterations => infinity,
        symbol Description => "",
        symbol BarCharacter => "▆",
    }
)
progressBar Iterator := ProgressBar => opts -> (theIterator) -> (
    theBar := new ProgressBar from theIterator;
    theBar.TotalIterations = opts.TotalIterations;
    theBar.Description = opts.Description;
    theBar.BarCharacter = opts.BarCharacter;
    theBar
)
progressBar List := ProgressBar => opts -> (theList) -> (
    progressBar(iterator theList, TotalIterations=>#theList, 
                Description=>opts.Description,
                BarCharacter=>opts.BarCharacter)
)
progressBar Set := ProgressBar => opts -> (theSet) -> (
    progressBar(toList theSet, TotalIterations=>#theSet, 
                Description=>opts.Description,
                BarCharacter=>opts.BarCharacter)
)

updateDisplay = method()
updateDisplay ProgressBar := (theBar) -> (
    descriptionString := if #(theBar.Description) != 0 then "(" | theBar.Description | ")" else "";

    -- Format progress bar itself.
    barString := if theBar.TotalIterations < infinity then (
        totalBlocks := 36;
        percentDone := numeric theBar.cache.CurrentIndex / theBar.TotalIterations;
        numFilled := floor(percentDone * totalBlocks);
        numEmpty := totalBlocks - numFilled;
        
        filledBlocks := concatenate(numFilled:theBar.BarCharacter);
        emptyBlocks := concatenate(numEmpty:" ");
        concatenate("|", filledBlocks, emptyBlocks, "|")
    )
    else "";

    -- Format the total completion indicators.
    if theBar.TotalIterations < infinity then (
        percentageString := toString(round(100*percentDone)) | "%";
        denominatorString := concatenate("/", toString(theBar.TotalIterations));
    )
    else (
        percentageString = "";
        denominatorString = "";
    );
    fractionString := toString(theBar.cache.CurrentIndex) | denominatorString;

    -- Do some time formatting.
    -- We want a display to look like [elapsed time < est. time remaining,  iterations / unit of time]
    -- CAVEAT: We only display times up to DAYS:HOURS:MINUTES:SECOND
    formatTime := (someTime) -> (
        -- Determine the time-scale
        wholeTime := round someTime;
        (totalMinutes, numSeconds) := quotientRemainder(wholeTime, 60);
        (totalHours, numMinutes) := quotientRemainder(totalMinutes, 60);
        (numDays, numHours) := quotientRemainder(totalHours, 24);

        padZeros := (someInteger) -> (
            if someInteger >= 10 then (toString someInteger)
            else ("0" | toString(someInteger))
        );
        dayString := if numDays > 0 then concatenate(padZeros numDays, ":") else "";
        hourString := if numHours > 0 then concatenate(padZeros numHours, ":") else "";
        concatenate(dayString, hourString, padZeros numMinutes, ":", padZeros numSeconds)
    );
    timeElapsedString := formatTime theBar.cache.TotalTimeElapsed;
    remainingTimeString := if theBar.cache.EstimatedRemainingTime =!= null then formatTime theBar.cache.EstimatedRemainingTime else "?";
    rateString := if theBar.cache.AverageTimePerIteration =!= null then (
                    -- Determine the time-scale.
                    (totalMinutes, numSeconds) := quotientRemainder(round theBar.cache.AverageTimePerIteration, 60);
                    (totalHours, numMinutes) := quotientRemainder(totalMinutes, 60);
                    (numDays, numHours) := quotientRemainder(totalHours, 24);
                    timescales := {numDays, numHours, numMinutes, numSeconds};
                    timescaleUnits := {"days", "hr", "min", "s"};
                    unitIndex := position(timescales, t -> not zero t);
                    unitIndex = if unitIndex =!= null then unitIndex else -1;
                    secondsToUnitFactor := (accumulate({1/24, 1/60, 1/60, 1}, numeric 1, (x,y) -> x*y))#unitIndex;
                    unitString := timescaleUnits#unitIndex;


                    if theBar.cache.AverageTimePerIteration >= 1 then (
                        concatenate(toString round(2, theBar.cache.AverageTimePerIteration * secondsToUnitFactor), unitString | "/it")
                    )
                    else (
                        if theBar.cache.AverageTimePerIteration > 0 then (
                            concatenate(toString round(2, 1 / (theBar.cache.AverageTimePerIteration  * secondsToUnitFactor)), "it/" | unitString)
                        )
                        else (
                            concatenate(toString theBar.cache.CurrentIndex, "it/" | unitString)
                        )
                    )
                )
                else ("?");
    timeEstimateString := concatenate("[", formatTime theBar.cache.TotalTimeElapsed, "<", remainingTimeString, ", ", rateString, "]");

    progressBarString := concatenate(descriptionString,
                                     " ",
                                     percentageString,
                                     " ", 
                                     barString, 
                                     " ",
                                     fractionString,
                                     " ",
                                     timeEstimateString
    );
    
    -- Print to the user.
    ending := if theBar.cache.CurrentIndex == theBar.TotalIterations then endl else flush;
    stdio << "\r" << progressBarString << ending;
)

next ProgressBar := (theBar) -> (
    -- Calculate the time deltas.
    if theBar.cache.CurrentIndex > 0 then (
        theBar.cache.TotalTimeElapsed = currentTime() - theBar.cache.StartTime;
    )
    else (
        theBar.cache.StartTime = currentTime();
        theBar.cache.TotalTimeElapsed = 0;
    );
    -- Estimate the remaining time.
    if theBar.cache.CurrentIndex > 0 then (
        theBar.cache.AverageTimePerIteration = numeric theBar.cache.TotalTimeElapsed / theBar.cache.CurrentIndex;
        if theBar.TotalIterations < infinity then (
            remainingIterations := theBar.TotalIterations - theBar.cache.CurrentIndex + 1;
            theBar.cache.EstimatedRemainingTime = theBar.cache.AverageTimePerIteration * remainingIterations;
        );
    );

    updateDisplay theBar;
    theBar.cache.CurrentIndex += 1;
    next theBar.Iterable
)

iterator ProgressBar := (theBar) -> Iterator (
    () -> (
        if theBar.cache.CurrentIndex <= theBar.TotalIterations then (next theBar)
        else (StopIteration)
    )
)
