restart
importFrom_Core {"markdown"}

ver = "1.17"
illinois = "https://faculty.math.illinois.edu/Macaulay2/doc/Macaulay2-" | ver | "/share"

changelog = markdown help("changes, " | ver);
changelog = replace("/home/.*?/common/share", illinois, changelog);
changelog = replace("common/share", illinois, changelog);
changelog = replace("https://faculty.math.illinois.edu", "http://www2.macaulay2.com", changelog);
("changelog-" | ver | ".md") << changelog << close


