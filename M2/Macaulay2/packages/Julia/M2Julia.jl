"""
    M2Julia

Internal helpers for the Macaulay2 Julia package.
"""

module M2Julia
# dict of known objects so they don't get garbage collected
const known_objects = Dict{Int64, Any}()

# returns the key used for the object's type in juliaToM2Functions
value_key(x::Any) = -1

function set_value_key(T::Type, key::Int)
    @eval value_key(::$T) = $key
    nothing
end

# integer conversion
function zz_to_int128(hi::Int64, lo::UInt64)
    (Int128(hi) << 64) | lo
end
function zz_to_bigint(ptr::Ptr{Cvoid})
    n = BigInt()
    ccall((:__gmpz_set, Base.GMP.libgmp), Cvoid,
          (Ref{BigInt}, Ptr{Cvoid}), n, ptr)
    n
end
function int128_to_zz(x::Int128)
    ((x >> 64) % Int64, x % UInt64)
end
function uint128_to_zz(x::UInt128)
    ((x >> 64) % UInt64, x % UInt64)
end

# float conversion
function rr_to_bigfloat(ptr::Ptr{Cvoid}, prec::Clong)
    x = BigFloat(; precision = prec)
    ccall((:mpfr_set, Base.MPFR.libmpfr), Cint,
          (Ref{BigFloat}, Ptr{Cvoid}, Base.MPFR.MPFRRoundingMode),
          x, ptr, Base.MPFR.MPFRRoundNearest)
    x
end
function bigfloat_to_rr(x::BigFloat)
    Ptr{Cvoid}(Base.unsafe_convert(Ref{BigFloat}, Base.cconvert(Ref{BigFloat}, x)))
end

# named tuples
function options_to_namedtuple(name_strs::Tuple{Vararg{String}}, values::Tuple)
    names = map(Symbol, name_strs)
    NamedTuple{names}(values)
end

end
