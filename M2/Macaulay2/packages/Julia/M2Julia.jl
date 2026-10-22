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
end
