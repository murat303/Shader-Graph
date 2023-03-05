using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class RotateCubeMap : MonoBehaviour
{
    public float speed = 1;
    private MeshRenderer _meshRenderer;

    private void Awake()
    {
        _meshRenderer = GetComponent<MeshRenderer>();
    }

    public void Update()
    {
        _meshRenderer.material.SetFloat("_Rotation", (Time.time * speed) % 360);
    }
}
