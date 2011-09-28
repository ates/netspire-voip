CREATE LANGUAGE plpgsql;

CREATE SEQUENCE voip_routes_id_seq START 1;
CREATE SEQUENCE voip_plans_id_seq START 1;
CREATE SEQUENCE voip_classes_id_seq START 1;
CREATE SEQUENCE voip_rules_id_seq START 1;

CREATE TABLE voip_routes(
    id INTEGER NOT NULL DEFAULT NEXTVAL('voip_routes_id_seq'::regclass),
    route VARCHAR NOT NULL,
    description TEXT,
    active BOOLEAN DEFAULT TRUE
);

CREATE TABLE voip_plans(
    id INTEGER NOT NULL DEFAULT NEXTVAL('voip_plans_id_seq'),
    name VARCHAR NOT NULL,
    description TEXT,
    service_tax NUMERIC(20, 10) DEFAULT 0,
    connect_fee NUMERIC(20, 10) DEFAULT 0, 
    free_seconds INTEGER DEFAULT 0,
    active BOOLEAN DEFAULT TRUE
);

CREATE TABLE voip_classes(
    id INTEGER NOT NULL DEFAULT NEXTVAL('voip_classes_id_seq'::regclass),
    voip_plan_id INTEGER NOT NULL,
    name VARCHAR NOT NULL,
    description TEXT,
    cost NUMERIC(20, 10) NOT NULL
);

CREATE TABLE voip_rules(
    id INTEGER NOT NULL DEFAULT NEXTVAL('voip_rules_id_seq'::regclass),
    voip_class_id INTEGER NOT NULL,
    voip_route_id_src INTEGER NOT NULL,
    voip_route_id_dst INTEGER NOT NULL
);

CREATE OR REPLACE FUNCTION match(VARCHAR, VARCHAR, VARCHAR) RETURNS TABLE (
    plan VARCHAR,
    cost FLOAT,
    service_tax FLOAT,
    connect_fee FLOAT,
    free_seconds INTEGER) AS $$
BEGIN
    RETURN QUERY SELECT t1.name, t1.cost::FLOAT, t0.service_tax::FLOAT, t0.connect_fee::FLOAT, t0.free_seconds FROM voip_plans t0
        LEFT OUTER JOIN voip_classes t1 ON t1.voip_plan_id = t0.id

        LEFT OUTER JOIN voip_rules t2 ON t2.voip_class_id = t1.id

        LEFT OUTER JOIN voip_routes t3 ON t2.voip_route_id_src = t3.id OR t2.voip_route_id_dst = t3.id

        WHERE t0.name = $1 AND t0.active = TRUE AND
            ($2 ~ (SELECT route FROM voip_routes WHERE id = t2.voip_route_id_src)
        AND 
            $3 ~ (SELECT route FROM voip_routes WHERE id = t2.voip_route_id_dst)) LIMIT 1;
end;
$$ LANGUAGE plpgsql;

-- Test data
INSERT INTO voip_routes(route, description) VALUES('^066.*', 'MTS Jeans');
INSERT INTO voip_routes(route, description) VALUES('^097.*', 'KyevStar DJuice');

INSERT INTO voip_plans(name) VALUES('Ultimate');

INSERT INTO voip_classes(voip_plan_id, name, cost) VALUES(1, 'From MTS Jeans to KyevStar DJuice', 10);
INSERT INTO voip_classes(voip_plan_id, name, cost) VALUES(1, 'From KyevStar DJuice to From MTS Jeans', 20);

INSERT INTO voip_rules(voip_class_id, voip_route_id_src, voip_route_id_dst) VALUES(1, 1, 2);
INSERT INTO voip_rules(voip_class_id, voip_route_id_src, voip_route_id_dst) VALUES(2, 2, 1);

