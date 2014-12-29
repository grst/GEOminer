-- GDS
BEGIN;

-- numeric matrix
CREATE TABLE "numeric_matrix" (
  id integer NOT NULL PRIMARY KEY,
  nrow integer  NOT NULL,
  ncol integer  NOT NULL,
  "name" varchar(50),
  "desc" varchar(200)
);

CREATE TABLE "numeric_matrix_row" (
    id integer NOT NULL PRIMARY KEY,
    matrix_id integer NOT NULL REFERENCES "numeric_matrix" ("id"),
    ind integer NOT NULL,
    "name" varchar(50) NOT NULL,
    "desc" varchar(200)
);

CREATE TABLE "numeric_matrix_column" (
    id integer NOT NULL PRIMARY KEY,
    matrix_id integer NOT NULL REFERENCES "numeric_matrix" ("id"),
    ind integer  NOT NULL,
    "name" varchar(50) NOT NULL,
    "desc" varchar(200)
);

CREATE TABLE "numeric_matrix_data" (
    id integer NOT NULL PRIMARY KEY,
    matrix_id integer NOT NULL REFERENCES "numeric_matrix" ("id"),
    row_id integer  NOT NULL REFERENCES "numeric_matrix_row" ("id"),
    col_id integer  NOT NULL REFERENCES "numeric_matrix_column" ("id"),
    value numeric
);

CREATE TABLE numeric_matrix_pca (
    id integer NOT NULL PRIMARY KEY,
    matrix_id integer NOT NULL REFERENCES "numeric_matrix" ("id"),
    pca_dimension integer NOT NULL,
    exp_var numeric NOT NULL,
    exp_var_perc numeric NOT NULL,
    cumulative_exp_var_perc numeric NOT NULL,
    UNIQUE (matrix_id, pca_dimension)
);

-- column-wise PCA
CREATE TABLE numeric_matrix_pca_score (
   numeric_matrix_data_id integer NOT NULL REFERENCES "numeric_matrix_data" ("id"),
   numeric_matrix_column_id integer NOT NULL REFERENCES "numeric_matrix_column" ("id"),
   pca_id integer NOT NULL REFERENCES "numeric_matrix_pca" ("id")
);


CREATE TABLE "gds_matrix" (
  matrix_id integer NOT NULL PRIMARY KEY REFERENCES "numeric_matrix" ("id"),
  gdsID varchar(50) NOT NULL UNIQUE,
  gpl varchar(50) NOT NULL
);

-- data structure for design and contrast matrix

CREATE TABLE design_matrix (
    id integer NOT NULL PRIMARY KEY REFERENCES "numeric_matrix" ("id"),
    exprs_matrix_id integer NOT NULL REFERENCES "numeric_matrix" ("id")
);

CREATE TABLE contrast_matrix (
    id integer NOT NULL PRIMARY KEY REFERENCES "numeric_matrix" ("id"),
    design_matrix_id integer NOT NULL REFERENCES "design_matrix" ("id")
);

CREATE TABLE limma_deg (
    id integer NOT NULL PRIMARY KEY,
    contrast_col_id integer NOT NULL REFERENCES "numeric_matrix_column" ("id"),
    exprs_row_id integer NOT NULL REFERENCES "numeric_matrix_row" ("id"),
    logFC numeric,
    AveExpr numeric,
    t numeric,
    p numeric,
    adjP numeric,
    B numeric
);

CREATE INDEX "numeric_matrix_row_mind" ON "numeric_matrix_row" (matrix_id);
CREATE INDEX "numeric_matrix_row_nind" ON "numeric_matrix_row" (name);
CREATE INDEX "numeric_matrix_column_mind" ON "numeric_matrix_column" (matrix_id);
CREATE INDEX "numeric_matrix_column_nind" ON "numeric_matrix_column" (name);
CREATE INDEX "numeric_matrix_data_mind" ON "numeric_matrix_data" (matrix_id);
CREATE INDEX "numeric_matrix_data_rind" ON "numeric_matrix_data" (row_id);
CREATE INDEX "numeric_matrix_data_cind" ON "numeric_matrix_data" (col_id);
CREATE INDEX "numeric_matrix_pca_mind" ON "numeric_matrix_pca" (matrix_id);
CREATE INDEX "numeric_matrix_pca_dind" ON "numeric_matrix_pca" (pca_dimension);
CREATE INDEX "numeric_matrix_pca_mdind" ON "numeric_matrix_pca" (matrix_id,pca_dimension);
CREATE INDEX "numeric_matrix_pca_score_mind" ON "numeric_matrix_pca_score" (numeric_matrix_data_id);
CREATE INDEX "numeric_matrix_pca_score_cind" ON "numeric_matrix_pca_score" (numeric_matrix_column_id);
CREATE INDEX "numeric_matrix_pca_score_pind" ON "numeric_matrix_pca_score" (pca_id);
CREATE INDEX "gds_matrix_gind" ON "gds_matrix" (gdsID);
CREATE INDEX "design_matrix_mid" ON "design_matrix" (exprs_matrix_id);
CREATE INDEX "contrast_matrix_mid" ON "contrast_matrix" (design_matrix_id);
CREATE INDEX "limma_deg_cind" ON "limma_deg" (contrast_col_id);
CREATE INDEX "limma_deg_eind" ON "limma_deg" (exprs_row_id);

COMMIT;
