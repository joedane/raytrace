import Base.enumerate
import Base.getindex

const MyFloat = Float64

struct Vec
    x::MyFloat
    y::MyFloat
    z::MyFloat
end

const BLACK = Vec(0, 0, 0)

struct Ray
    origin::Vec
   direction::Vec
end

abstract type Hitable end

mutable struct HitRecord
    hit_something::Bool
    t::MyFloat
    p::Vec
    incident::Ray
    normal::Vec
    object::Hitable
end

const ZERO_VEC = Vec(0, 0, 0)

HitRecord(ray::Ray) = HitRecord(false, 0.0, ZERO_VEC, ray, ZERO_VEC, BACKGROUND)

abstract type Material end

function scatter(m::Material, r::Ray, rec::HitRecord) end

function hit_object!(h::Hitable, r::Ray, t_min::MyFloat, t_max::MyFloat, rec::HitRecord) end

type Background <: Hitable 
    material::Material
end

struct Sphere <: Hitable
    center::Vec
    radius::MyFloat
    material::Material
end

type BackgroundMaterial <: Material end

const BACKGROUND = Background(BackgroundMaterial())

struct Lambertian <: Material 
    albedo::Vec
end

struct Metal <: Material
    albedo::Vec
    fuzz::MyFloat
end

struct Dielectric <: Material
    index::MyFloat
end

type World
    l::Vector{Sphere}
end

function enumerate(w::World)
    return Base.Iterators.enumerate(w.l)
end

function getindex(w::World, i::Int64)
    return getindex(w.l, i)
end

function scatter(obj::Lambertian, r::Ray, rec::HitRecord) 
    target = rec.p + rec.normal + random_in_unit_sphere()
    return Ray(rec.p, target-rec.p)
end

function reflect(v::Vec, n::Vec)
    return v - 2*dot(v,n)*n
end

function scatter(obj::Metal, r::Ray, hit::HitRecord)
    reflected = reflect(unit_vector(r.direction), hit.normal)
    scattered = Ray(hit.p, reflected + obj.fuzz*random_in_unit_sphere())
    if (dot(scattered.direction, hit.normal) > 0)
        return scattered
    else
        return Nullable{Tuple{Vec, Ray}}()
    end
end

function refract(v::Vec, n::Vec, ni_over_nt::MyFloat)
#    println(STDERR, "v: $v")
#    println(STDERR, "n: $n")
#    println(STDERR, "ni/nt: $ni_over_nt")

    uv = unit_vector(v)
    dt = dot(uv, n)

    discriminant = 1.0 - ni_over_nt*ni_over_nt*(1-dt*dt)
    if (discriminant > 0)
#        refr = ni_over_nt*(uv - n*dt) - n*sqrt(discriminant)
        refr = ni_over_nt*(uv) - n*sqrt(discriminant)
#        println(STDERR, "refr: $refr")
        return (refr, true)
    else
        return (Vec(0, 0, 0.0013), false)
    end
end

function schlick(cosine::MyFloat, idx::MyFloat)
    r0 = (1-idx) / (1+idx)
    r0 = r0*r0
    return r0 + (1-r0) * (1-cosine)^5
end

function scatter(obj::Dielectric, r::Ray, hit::HitRecord)
    attenuation = Vec(1, 1, 1)
    reflected = reflect(r.direction, hit.normal)
    
    if (dot(r.direction, hit.normal) > 0)
        outward_normal = -1 * hit.normal
        ni_over_nt = obj.index
        cosine = obj.index * dot(r.direction, hit.normal) / length(r.direction)
    else
        outward_normal = hit.normal
        ni_over_nt = 1.0 / obj.index
        cosine = -1 * dot(r.direction, hit.normal) / length(r.direction)
    end
    
    refracted = refract(r.direction, outward_normal, ni_over_nt)
    if (!isnull(refracted))
        if (rand() < schlick(cosine, obj.index))
            return (attenuation, Ray(hit.p, reflected))
        else
            return (attenuation, Ray(hit.p, get(refracted)))
        end
    else
        return (attenuation, Ray(hit.p, reflected))
    end
end


struct Camera 
    origin::Vec
    horizontal::Vec
    vertical::Vec
    lower_left_corner::Vec
    u::Vec
    v::Vec
    w::Vec
    lens_radius::MyFloat
end

function Camera()
    lookfrom = Vec(3, 3, 10)
    lookat = Vec(0, 0, -1)
    Camera(lookfrom, lookat,
           Vec(0, 1, 0),
           20.0, 2.0, 0.2, length(lookfrom-lookat))
end

function Camera(lookfrom::Vec, lookat::Vec,
                vup::Vec, vfov::MyFloat, aspect_ratio::MyFloat,
                aperture::MyFloat, focus_dist::MyFloat)

    theta = vfov*pi/180
    half_height = tan(theta/2)
    half_width = aspect_ratio * half_height
    origin = lookfrom
    w = unit_vector(lookfrom - lookat)
    u = unit_vector(mycross(vup, w))
    v = mycross(w, u)

    Camera(origin,
           2*half_width*focus_dist*u,
           2*half_height*focus_dist*v,
           origin - half_width*focus_dist*u - half_height*focus_dist*v - focus_dist*w,
           u, v, w,
           aperture/2)
end

#=
function get_ray(c::Camera, u::MyFloat, v::MyFloat)
    Ray(c.origin, c.lower_left_corner + u*c.horizontal + v*c.vertical - c.origin)
end
=#

function get_ray(c::Camera, s::MyFloat, t::MyFloat)
    rd = c.lens_radius*random_in_unit_disk()
    offset = c.u*rd.x + c.v*rd.y
    return Ray(c.origin + offset, c.lower_left_corner + s*c.horizontal + t*c.vertical - c.origin - offset)
end

function length(vec::Vec)
    sqrt(squared_length(vec))
end

function squared_length(vec::Vec)
    vec.x*vec.x + vec.y*vec.y + vec.z*vec.z
end

function Base.:+(v1::Vec, v2::Vec)
    Vec(v1.x + v2.x, v1.y + v2.y, v1.z + v2.z)
end

function make_unit_vector(v::Vec)
    k = 1.0 / sqrt(v.x*v.x + v.y*v.y + v.z*v.z)
    Vec(v.x*k, v.y*k, v.z*k)
end

function unit_vector(v::Vec)
    v / length(v)
end

# order matters here
function Base.:*(v::Vec, f::Number)
    Vec(f*v.x, f*v.y, f*v.z)
end

function Base.:*(f::Number, v::Vec)
    Vec(f*v.x, f*v.y, f*v.z)
end

function Base.:*(v1::Vec, v2::Vec)
    Vec(v1.x*v2.x, v1.y*v2.y, v1.z*v2.z)
end

function Base.:+(v1::Vec, v2::Vec)
    Vec(v1.x+v2.x, v1.y+v2.y, v1.z+v2.z)
end

function Base.:-(v1::Vec, v2::Vec)
    Vec(v1.x-v2.x, v1.y-v2.y, v1.z-v2.z)
end

function Base.:/(v1::Vec, v2::Vec)
    Vec(v1.x / v2.x, v1.y / v2.y, v1.z / v2.z)
end

function Base.:/(v1::Vec, f::Number)
    Vec(v1.x / f, v1.y / f, v1.z / f)
end

function dot(v1::Vec, v2::Vec)
    v1.x*v2.x + v1.y*v2.y + v1.z*v2.z
end

function mycross(v1::Vec, v2::Vec)
    Vec(v1.y*v2.z - v1.z*v2.y,
        v1.z*v2.x - v1.x*v2.z,
        v1.x*v2.y - v1.y*v2.x)
end

function point_at_parameter(r::Ray, f::MyFloat)
    r.origin + r.direction*f
end

function hit_object!(s::Sphere, r::Ray, 
                     t_min::MyFloat, t_max::MyFloat, rec::HitRecord)
   
    oc = r.origin - s.center
    a = dot(r.direction, r.direction)
    b = dot(oc, r.direction)
    c = dot(oc, oc) - s.radius*s.radius
    discriminant = b*b - a*c
    if (discriminant > 0) 
        temp = (-b - sqrt(b*b-a*c))/a
        if (temp < t_max && temp > t_min) 
            hitpoint = point_at_parameter(r, temp)
            rec.hit_something = true
            rec.t = temp
            rec.p = hitpoint
            rec.incident = r
            rec.normal = unit_vector(hitpoint - s.center)
            rec.object = s
        else
            temp = (-b + sqrt(b*b-a*c))/a
            if (temp < t_max && temp > t_min)
                hitpoint = point_at_parameter(r, temp)
                rec.hit_something = true
                rec.t = temp
                rec.p = hitpoint
                rec.incident = r
                rec.normal = unit_vector(hitpoint - s.center)
                rec.object = s
            end
        end
    end
end


function find_nearest_hit(world::World,
                          r::Ray,
                          t_min::MyFloat,
                          t_max::MyFloat
                          )
    
    closest_so_far = t_max
    hitrec = HitRecord(r)
    for (i, hitable) in enumerate(world)
        hit_object!(hitable, r, t_min, closest_so_far, hitrec)
        if (hitrec.hit_something)
            closest_so_far = hitrec.t
        end
    end
    return hitrec
end


function background_color(r::Ray)
    dir = unit_vector(r.direction)
    t = 0.5*(dir.y+1)
    return (1-t)*Vec(1, 1, 1) + t*Vec(0.5, 0.7, 1.0)
end

function make_big_world()
    v = Vector{Sphere}()
    push!(v, Sphere(Vec(0, -1000, 0), 1000, Lambertian(Vec(0.5, 0.5, 0.5))))
    for a in -11:10, b in -11:10
        material = rand()
        center = Vec(a+0.9*rand(), 0.2, b+0.9*rand())
        if (length(center-Vec(4, 0.2, 0)) > 0.9)
            if (material < 0.5)
                push!(v, Sphere(center, 0.2, Lambertian(Vec(rand()*rand(), rand()*rand(), rand()*rand()))))
#                push!(v, Sphere(center, 0.2, Lambertian(Vec(64.0/255, 224.0/255, 208.0/255))))
            elseif (material < 0.75)
                push!(v, Sphere(center, 0.2, Metal(Vec(0.5*(1+rand()), 0.5*(1+rand()), 0.5*(1+rand())), 0.5*rand())))
            else
                push!(v, Sphere(center, 0.2, Dielectric(1.5)))
            end
        end
    end
    push!(v, Sphere(Vec(4, 1, 0.5), 1.0, Dielectric(1.5)))
    push!(v, Sphere(Vec(-4, 1, 0), 1.0, Lambertian(Vec(0.4, 0.2, 0.1))))
    push!(v, Sphere(Vec(0, 1, -0.5), 1.0, Metal(Vec(64.0/255, 224.0/255, 208.0/255), 0)))
    return World(v)
end

function make_default_world()
    return World([
        Sphere(Vec(0, 0, -1), 0.5, Lambertian(Vec(0.8, 0.3, 0.3))),
        Sphere(Vec(0, -100.5, -1), 100, Lambertian(Vec(0.8, 0.8, 0))),
        Sphere(Vec(0, 0, -10), 0.5, Lambertian(Vec(0, 0, 1))),
        Sphere(Vec(1, 0, -1), 0.5, Metal(Vec(0.8, 0.6, 0.2), 0)),
        Sphere(Vec(-1, 0, -1), 0.5, Dielectric(1.4)),
#        Sphere(Vec(-1, 0, -1), -0.45, Dielectric(1.2))
    ])
end

function render(f::IO, c::Camera)
    render(f, make_default_world(), c)
end

function render(f::IO)
    render(f, make_default_world(), Camera())
end

function render(f::IO, world::World)
    render(f, world, Camera())
end

function render(f::IO, world::World, camera::Camera)
    nx = 800
    ny = 400
    ns = 50
    write(f, "P3\n$nx $ny\n255\n")
    for j = ny-1:-1:0, i = 0:nx-1
        color = Vec(0, 0, 0)
        for s = 0:ns-1
            u = (i + rand()) / nx
            v = (j + rand()) / ny
            r = get_ray(camera, u, v)
            #color += color_hitlist(r, world)
            #color += color_matte(r, world)
            color += get_color_for_ray(r, world, 0)
        end
        color /= ns
        color = Vec(sqrt(color.x), sqrt(color.y), sqrt(color.z))
        @printf(f, "%d %d %d\n", 
                255.99*color.x, 255.99*color.y, 255.99*color.z)
    end
end

function make_background_hitlist(f::IO)
    nx = 400
    ny = 200
    write(f, "P3\n$nx $ny\n255\n")
    lower_left_corner = Vec(-2, -1, -1)
    horizontal = Vec(4, 0, 0)
    vertical = Vec(0, 2, 0)
    origin = Vec(0, 0, 0)
    world = Hitable[
        Sphere(Vec(0, 0, -1), 0.5),
        Sphere(Vec(0, -100.5, -1), 100)
    ]
    for j = ny-1:-1:0, i = 0:nx-1
        u = convert(MyFloat, i) / nx
        v = convert(MyFloat, j) / ny
        r = Ray(origin, lower_left_corner + u*horizontal + v*vertical)
        color = color_hitlist(r, world)
        @printf(f, "%d %d %d\n", 
                255.99*color.x, 255.99*color.y, 255.99*color.z)
    end
end

function make_background(f::IO)
    nx = 400
    ny = 200
    write(f, "P3\n$nx $ny\n255\n")
    lower_left_corner = Vec(-2, -1, -1)
    horizontal = Vec(4, 0, 0)
    vertical = Vec(0, 2, 0)
    origin = Vec(0, 0, 0)
    for j = ny-1:-1:0, i = 0:nx-1
        u = convert(MyFloat, i) / nx
        v = convert(MyFloat, j) / ny
        r = Ray(origin, lower_left_corner + u*horizontal + v*vertical)
        color = color_sphere_normals(r)
        @printf(f, "%d %d %d\n", 
                255.99*color.x, 255.99*color.y, 255.99*color.z)
    end
end

function hit_sphere_p(center::Vec, radius::MyFloat, r::Ray) 

    oc = r.origin - center
    a = dot(r.direction, r.direction)
    b = 2 * dot(r.direction, oc)
    c = dot(oc, oc) - radius*radius
    discriminant = b*b - 4*a*c
    return discriminant >= 0.0
end

function hit_sphere(center::Vec, radius::MyFloat, r::Ray)
    oc = r.origin - center
    a = dot(r.direction, r.direction)
    b = 2 * dot(oc, r.direction)
    c = dot(oc, oc) - radius*radius
    discriminant = b*b - 4*a*c
    if (discriminant < 0) 
        return -1.0
    else
        return (-b - sqrt(discriminant)) / (2 * a)
    end
end

function color_hitlist(r::Ray, l::Array{Hitable})
    hit = check_hitables(l, r, 0.0, typemax(MyFloat))
    if (!isnull(hit)) 
        return 0.5*Vec(hit.normal.x+1, hit.normal.y+1, hit.normal.z+1)
    else
        return background_color(r)
    end
end

function color_simple_sphere(r::Ray)

    if (hit_sphere_p(Vec(0.0, 0.0, -1.0), 0.6, r))
        return Vec(66.0/255, 244.0/255, 212.0/255)
    else
        return background_color(r)
    end
end

function color_sphere_normals(r::Ray)
    t = hit_sphere(Vec(0, 0, -1), 0.5, r)
    if (t > 0)
        N = unit_vector(point_at_parameter(r, t) - Vec(0, 0, -1))
        return 0.5*Vec(N.x+1, N.y+1, N.z+1)
    else
        return background_color(r)
    end
end

function color_matte(r::Ray, l::Array{Hitable})
    hit = check_hitables(l, r, 0.001, typemax(MyFloat))
    if (!isnull(hit))
        target = hit.p + hit.normal + random_in_unit_sphere()
        return 0.5*color_matte(Ray(hit.p, target-hit.p), l)
    else
        return background_color(r)
    end
end

function find_hit(ray::Ray, world::World, min::MyFloat, max::MyFloat)::HitRecord
    hitrec = find_nearest_hit(world, ray, min, max)
    if (!hitrec.hit_something)
        hitrec.object = BACKGROUND
    end
    return hitrec
end

function get_object_color(object::Dielectric, hit::HitRecord, world::World, depth::Int)
    attenuation = Vec(1, 1, 1)
    incident = hit.incident

    if (dot(incident.direction, hit.normal) > 0)
        outward_normal = -1 * hit.normal
        ni_over_nt = object.index
        cosine = object.index * dot(incident.direction, hit.normal) / length(incident.direction)
    else
        outward_normal = hit.normal
        ni_over_nt = 1.0 / object.index
        cosine = -1 * dot(incident.direction, hit.normal) / length(incident.direction)
    end

    (refr_ray, was_refracted) = refract(incident.direction, outward_normal, ni_over_nt)
    if (was_refracted)
        reflect_prob = schlick(cosine, object.index)
    else
        reflect_prob = 1.0
    end
    
    if (rand() < reflect_prob)
        return get_color_for_ray(Ray(hit.p, reflect(unit_vector(incident.direction), hit.normal)),
                                 world, depth+1)
    else
        return get_color_for_ray(Ray(hit.p, refr_ray), world, depth+1)
    end
end

function get_object_color(object::Metal, hitrec::HitRecord, world::World, depth::Int)
    reflected = reflect(unit_vector(hitrec.incident.direction), hitrec.normal)
    scattered = Ray(hitrec.p, reflected + object.fuzz*random_in_unit_sphere())
    if (dot(scattered.direction, hitrec.normal) > 0)
        return object.albedo*get_color_for_ray(scattered, world, depth+1)
    else
        return ZERO_VEC
    end
end

function get_object_color(object::Lambertian, hitrec::HitRecord, world::World, depth::Int)
    target = hitrec.p + hitrec.normal + random_in_unit_sphere()
    return object.albedo*get_color_for_ray(Ray(hitrec.p, target-hitrec.p), world, depth+1)
end

function get_object_color(object::BackgroundMaterial, hitrec::HitRecord, world::World, depth::Int)
    return background_color(hitrec.incident)
end

function get_object_color(hitrec::HitRecord, world::World, depth::Int)
    return get_object_color(hitrec.object.material, hitrec, world, depth)
end

function get_color_for_ray(ray::Ray, world::World, depth::Int)::Vec
#    println(STDERR, "level $depth")
    if (depth >= 50)
        return BLACK
    else
        return get_object_color(find_hit(ray, world, 0.001, typemax(MyFloat)),
                                world, depth)
    end
end


function random_in_unit_sphere()
    unit = Vec(1, 1, 1)
    while true
        v = 2 * Vec(rand(), rand(), rand()) - unit
        if (squared_length(v) < 1)
            return v
        end
    end
end

function random_in_unit_disk() 
    unit = Vec(1, 1, 0)
    while true
        v = 2.0*Vec(rand(), rand(), 0) - unit
        if (squared_length(v) < 1)
            return v
        end
    end
end
