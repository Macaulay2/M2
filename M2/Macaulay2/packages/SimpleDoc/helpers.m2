wikipedia = method(TypicalValue => Hypertext)
wikipedia String          :=       title  -> HREF{ "https://en.wikipedia.org/wiki/" | title, title }
wikipedia(String, String) := (url, title) -> HREF{ "https://en.wikipedia.org/wiki/" |   url, title }

arXiv = method(TypicalValue => Hypertext)
arXiv String          :=  ref         -> HREF{ "https://arxiv.org/abs/" | ref, "arXiv:" | ref }
arXiv(String, String) := (ref, title) -> HREF{ "https://arxiv.org/abs/" | ref, title }

stacksProject = method(TypicalValue => Hypertext)
stacksProject(String, String) := (tag, title) -> HREF{ "https://stacks.math.columbia.edu/tag/" | tag, title }
