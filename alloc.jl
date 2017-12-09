
abstract type Test end

type Test1 <: Test
  a::Float64
end

function do_test(t::Test1)
    t.a = t.a+1
end

function get_test(t::Test1)
    return t.a
end

type Test2 <: Test
    b::Int
end

function do_test(t::Test2)
    t.b = t.b - 1
end

function get_test(t::Test2)
    return t.b
end

type MyContainer{T}
    x::T
end

function do_test(c::MyContainer{Test1})
    do_test(c.x)
end

function do_test(c::MyContainer{Test2})
    do_test(c.x)
end

function run_tests(t::Test)
    for n in 1:100000
        do_test(t)
    end
end

#function run_tests(t::Test2)
#    for n in 1:100000
#        do_test(t)
#    end
#end


function doit() 

    v = Vector(100)

    for n in 1:100
#        v[n] = Test1(n)
        v[n] = rand(Bool) ? Test1(n) : Test2(n)
#        v[n] = MyContainer{Test1}(Test1(n))
    end
    
    for k in 1:length(v)
        run_tests(v[k])
    end

    x = 0
    for n in 1:length(v)
        x += get_test(v[n])
    end
    println("x = $x")
end
        
