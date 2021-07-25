#nb # %% CombinedParsers [markdown] {"slideshow": {"slide_type": "slide"}}
# # Parse and Broker Messages with CombinedParsers
# Faster write fast parsers
using CombinedParsers                   ## JuliaCon 2021


speaker = "Gregor Kappler
psychometric scientist and programmer, independent";



import CombinedParsers.Regexp: word, words, whitespace # hide

# This workshop is about using the julia package CombinedParsers.
# I like the brief readable julia language a lot.
# This title slide demonstrates why.

person_background = Sequence(
    :name => !words, whitespace, :lastname => !word, "\n",
    :background => !words)

person_background(speaker)

#nb # %% [markdown] {"slideshow": {"slide_type": "notes"}}
# ## Parsing
# *converting vectors (strings/binary) to any type with julia*
# and CombinedParsers
#
# Are julia types cool for parsing?

# ## Workshop questions
#
# Are Julia types & method dispatch suited
#
# -   for converting Strings to anything fast?
# -   to build efficient string/message processing systems?

# ## Workshop Outline:
# 1.  Message Broker like Kafka
# 2.  Writing recursive CombinedParsers (ENBF)
# 3.  CombinedParsers internals: States and optimization
#     Contributers welcome.

# # Part 1: Julia for message brokering?
#
# Julia
#
# -   established in distributed scientific computing.
# -   parametric types interesting for compiled parsers
# -   growing adoption
# -   opportunities in industry?
# Workshop notenook online at <https://github.com/gkappler/JuliaCon2021_CombinedParsers_workshop.jl>.
# **Please use julia 1.5** (1.6 issues with generated functions for long sequences).

# ## Usecase Example: Webserver Message Brokering
#
# Big Picture: 
#
# -   Event streaming is big business,
# -   Confluence uses Apache Kafka,
#     <img src="https://dz2cdn3.dzone.com/storage/article-thumb/12418930-thumb.jpg" width="200"/>
#     not hadoop, but another java elephant in solution zoo
#     <img src="https://cdn.worldvectorlogo.com/logos/hadoop.svg" width="200"/>
#
# Are Julia types & method dispatch
# suited to build an efficient distributed message broker/parser?

#nb # %% [markdown] {"slideshow": {"slide_type": "slide"}}
# ## Compiledparsers Performance: PCRE Benchmarks
# - `Regex` benchmarks: 86.0ns-521.0ns.
# - `Regcomb` benchmarks: 34.0ns-19704.0ns.
# - average ratio of `time_Recomb/time_Regex`: 1.26.
# - 59.0% of benchmarks faster with `Regcomb?`
# ![](https://gkappler.github.io/CombinedParsers.jl/dev/man/log_btimes.png)

# ### Know/interested in Kafka?
#
# Kafka terminology: 
#
# #### Message Broker
# 
# ##### Producer
# > The producer sends data directly to the broker that is the leader for the partition without any intervening routing tier.
#
# <https://kafka.apache.org/documentation/#theproducer>
#
# ##### Consumer
# > The Kafka consumer works by issuing "fetch" requests to the brokers leading the partitions it wants to consume. The consumer specifies its offset in the log with each request and receives back a chunk of log beginning from that position. The consumer thus has significant control over this position and can rewind it to re-consume data if need be.
#
# <https://kafka.apache.org/documentation/#theconsumer>

# ##### How can Message Broker processing be done in julia?
# > Push vs. pull
# > 
# > An initial question we considered is whether consumers should pull data from brokers or brokers should push data to the consumer. In this respect Kafka follows a more traditional design, shared by most messaging systems, where data is pushed to the broker from the producer and pulled from the broker by the consumer.
# <https://kafka.apache.org/documentation/#design_pull>
#
# Julia `RemoteChannel` has same design, compiled and distributed.
# (Julia Channels oriented at jobs/tasks/parallelism <https://docs.julialang.org/en/v1/manual/asynchronous-programming/#More-on-Channels>)
using Distributed
# addprocs(2); # add worker processes
webserver_logs = RemoteChannel()

# ### POC Focus:
# distributed compiled processing/parsing.
#
# ### POC limitation:
# in memory (no stream persistence in `RemoteChannel`).

# ## Example data: log file
# Webserver logs <https://github.com/elastic/examples/tree/master/Common%20Data%20Formats>

false && let logpath = "https://github.com/elastic/examples/raw/master/Common%20Data%20Formats"
    download("$logpath/apache_logs/apache_logs", "./apache_logs")
    download("$logpath/nginx_logs/nginx_logs", "./nginx_logs")
end

# ### Producers
# <https://stackoverflow.com/questions/67348301/julia-iterator-which-parses-each-line-in-file>
# 1. read line by line
# 2. parse a NamedTuple
# 3. geoip
#
# #### Waiting file line iterator:
#
# Modified from `Base.EachLine`
@everywhere struct EachLineFollow{IOT <: IO}
    stream::IOT
    ondone::Function
    keep::Bool
    wait::Float64
    EachLineFollow(stream::IO=stdin; wait=1.0, ondone::Function=()->nothing, keep::Bool=false) =
        new{typeof(stream)}(stream, ondone, keep, wait)
end

@everywhere function Base.iterate(itr::EachLineFollow, state=nothing)
    while eof(itr.stream)
        ## nicely waiting itr.wait sec when eof
        sleep(itr.wait)
    end
    (readline(itr.stream, keep=itr.keep), nothing)
end

@everywhere Base.eltype(::Type{<:EachLineFollow}) = String

@everywhere Base.IteratorSize(::Type{<:EachLineFollow}) = SizeUnknown()

# #### produce log lines
# `make_jobs(n)` Following similar to example in <https://docs.julialang.org/en/v1/manual/distributed-computing/>
# More on distributed computing: 
# <https://docs.julialang.org/en/v1/stdlib/Distributed/#Distributed.RemoteChannel>
# 
@everywhere function put_lines!(f::IO, broker, a...)
    for l in EachLineFollow(f)
        put!(broker, (l,a...))
    end
end
@everywhere put_lines!(f::String, broker, a...) =
    put_lines!(open(f), broker, f, a...)

@async put_lines!("./apache_logs", webserver_logs)
@async put_lines!("./nginx_logs", webserver_logs)

logline, source = ("""
83.149.9.216 - - [17/May/2015:10:05:03 +0000] "GET /presentations/logstash-monitorama-2013/images/kibana-search.png HTTP/1.1" 200 203023 "http://semicomplete.com/presentations/logstash-monitorama-2013/" "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_9_1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/32.0.1700.77 Safari/537.36"
""", "apache") # take!(webserver_logs)

# ### Transformation
# Consumer of `event::String` and producer of parsings, similar to
# <https://kafka.apache.org/documentation/#connect_transforms>

using CombinedParsers

# can be combined with

using TextParse

decimal_byte = parser(TextParse.Numeric(UInt8))
datetime = parser(TextParse.DateTimeToken(TextParse.DateFormat("d/u/Y:H:M:S")))
decimal_byte("10"), datetime("17/May/2015:10:05:43")

# ##### Matching IP Adresses

ipv4 = join(Repeat(4,4,decimal_byte),'.')
ipv6 = join(Repeat(8,8,integer_base(16)),':')


# `!` is for capturing matched `SubString` (Syntax inspired by scala fastparse)
# Either: format alternatives 

ip = !Either(ipv4, ipv6)

# String macro above executed match and get:
using BenchmarkTools

m = match(ipv6, "2003:ef:272a:5900:138a:7ce1:2ed:cd2")
@btime get(m)

# Matching an ip pattern as `SubString` is faster:

m = match(ip, "2003:ef:272a:5900:138a:7ce1:2ed:cd2")
@btime get(m)

# Defining parsers without syntax gives less (default) logging
# `@syntax name = expr` is a convenience macro defining `name=with_name(name,expr)` and custom parsing macro `@name_str`.

number = TextParse.Numeric(Int)
nospace = !Repeat(CharNotIn(' '))
escapedquotes = !Repeat(Either("\\\"", CharNotIn('"')))

@syntax apache_logline = Sequence(
    :ip               => ip,
    " ",
    :identd           => Either("-" => missing, nospace),
    " ",
    :user             => Either("-" => missing, nospace),
    " [",
    :date             => datetime, " +", :timezone => number,
    "] \"",
    :req_method       => !Either("GET", "POST", "HEAD", "OPTIONS"),
    ' ',
    :req_url          => nospace,
    " HTTP/",
    :req_http_version => TextParse.Numeric(Float64),
    "\" ",
    :status           => number,
    ' ',
    :size             => Either(number, '-' =>0 ) ,
    ' ',
    '"',
    :url              => escapedquotes,
    "\" \"", 
    :browser          => escapedquotes, '"'
)


# #### Logging
# can be done selectively
apache_logline(logline, log=[:ip]);

# and fully
apache_logline(logline, log=true)

function parse_logline((event,file), parser, producer)
    tevent = tryparse(parser, event)
    if tevent === nothing
        tryparse(parser, event, log=true)
        println(event)
    else
        put!(producer,(tevent, file))
        tevent, file
    end
end

logtable_rows = RemoteChannel()
parse_logline((logline, source), apache_logline, logtable_rows)


# tryparse all formats

function take_map!(f::Function, broker, a...; kw...)
    while true
        event = take!(broker)
        f(event, a...; kw...)
    end
end



@async take_map!(parse_logline,  # map function
                 webserver_logs, # input
                 apache_logline, # further arguments to parse_logline...
                 logtable_rows)


# ### Transformation: IP Geolocation (privacy obfuscation)
# <https://www.maketecheasier.com/ip-address-geolocation-lookups-linux/>
# no city level
`geoiplookup 162.158.91.43` |> run

# We parse the `geoiplookup` output (on city level, if available)
`geoiplookup 8.8.8.8` |> run



geoiplookup_out = Either(
    Sequence(6,
             "GeoIP Country Edition: ", inline, newline,
             "GeoIP City Edition, Rev 0: ", NegativeLookahead("IP Address not found"), !inline, newline),
    Sequence(2,
             "GeoIP Country Edition: ", !inline, newline)
)

function geoip(ip::AbstractString)
    read(`geoiplookup $ip`, String) |> geoiplookup_out
end
iplookup = Either(map(geoip, !ipv4),
                  ipv6 => "no GeoIP for IPv6")
geoip("162.158.91.43"), geoip("8.8.8.8")

# IPv6 is not supported by geoiplookup
function geoip(x::NamedTuple)
    (; geoip=iplookup(x.ip),
     ( p => getproperty(x,p)
       for p in propertynames(x)
           if p != :ip)... )
end
geoip((ip="162.158.91.43", some=1))

geoip(x::Tuple{<:NamedTuple,<:AbstractString}) =
    (geoip(x[1]),x[2])

geoip(x::Tuple{<:NamedTuple,<:AbstractString}, producer::RemoteChannel) =
    put!(producer, geoip(x))

#

# x = take!(logtable_rows)

geoip_rows = RemoteChannel()

@async take_map!(geoip,  # map function
                 logtable_rows, # input
                 geoip_rows)

# x = take!(geoip_rows)

# ### Parallelism
# Useful for indexing use case with
# Wiktionary developing wikitext recursive parser <https://github.com/gkappler/WikitextParser.jl>

# ## Consumers and sinks:
# `RemoteChannel` can be mapped with any julia function, e.g.
# - store in Database with Lighthouse.jl
# - log file
# - dispatched to multiple consumers
# - gittrie.jl with csv (published soon)
# 

# ## Message brokering Proof of Concept 
# Comparing julia with java
# - using function dispatch in julia
# - java classes for Kafka transformations
# Julia is slicker (runtime, devtime and concepts)

# ### Prospect for message brokering in julia.
# #### Kafka Event streams
# Workshop focus on processing, not handling/storage.
#
# > A stream is the most important abstraction provided by Kafka Streams: it represents an unbounded, continuously updating data set.
# > A stream is an ordered, **replayable**, and **fault-tolerant** sequence of immutable data records, where a data record is defined as a key-value pair.
#
# <https://kafka.apache.org/11/documentation/streams/core-concepts#streams_topology>
#
# Serializing messages with offset indices, e.g. with <https://docs.julialang.org/en/v1/stdlib/Mmap/>, <https://github.com/JuliaData/MemPool.jl>
# Kafka streams feature interesting for julia `RemoteChannel`?
#
# Current business deal-breakers:
# -   no [processing guarantee](https://kafka.apache.org/11/documentation/streams/core-concepts#streams_processing_guarantee)
#
# No discussion of Kafka events ∈ topics, or partitions.


