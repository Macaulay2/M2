
end -- disable this test

docFile = openDatabase databaseFileName
time scanKeys(docFile,
     key -> (
	  doc := docFile#key;
	  if not match(doc,"goto *") then (
     	       fkey := formatDocumentTag value key;
	       if not allDoc#?fkey then(
		    doc = (
			 try value doc
			 else error ("error evaluating documentation string for ", toExternalString key)
			 );
		    linkFilename fkey;
		    allDoc#fkey = doc))))
close docFile
end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test missingDoc.out"
-- End:
